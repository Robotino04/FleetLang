#include <cstdlib>
#include <cstring>
#include <utility>
#include <vulkan/vulkan.hpp>
#include <iostream>
#include <vector>
#include <fstream>
#include <cassert>
#include <vulkan/vulkan_core.h>

#include <vulkan/vulkan_enums.hpp>
#include <vulkan/vulkan_handles.hpp>
#include <vulkan/vulkan_structs.hpp>
#include <vulkan/vulkan_to_string.hpp>

#define VK_CHECK(x)                                                                                         \
    do {                                                                                                    \
        VkResult err = x;                                                                                   \
        if (err) {                                                                                          \
            std::cerr << "Fatal : VkResult is \"" << vk::to_string((vk::Result)err) << "\" at " << __FILE__ \
                      << ":" << __LINE__ << "\n";                                                           \
            std::exit(-1);                                                                                  \
        }                                                                                                   \
    } while (0)


static const std::vector<const char*> validationLayers = {
    "VK_LAYER_KHRONOS_validation",
};

bool checkValidationLayerSupport() {
    uint32_t layerCount;
    vkEnumerateInstanceLayerProperties(&layerCount, nullptr);

    std::vector<VkLayerProperties> availableLayers(layerCount);
    vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.data());

    std::cout << "Available validation layers:\n";
    for (const auto& layerProperties : availableLayers) {
        std::cout << "- " << layerProperties.layerName << "\n";
    }

    std::cout << "Requested validation layers:\n";
    for (const auto& layerName : validationLayers) {
        std::cout << "- " << layerName << "\n";
    }

    for (const auto& layerName : validationLayers) {
        bool layerFound = false;

        for (const auto& layerProperties : availableLayers) {
            if (std::string(layerName) == layerProperties.layerName) {
                layerFound = true;
                break;
            }
        }

        if (!layerFound) {
            return false;
        }
    }


    return true;
}

std::vector<char> readSPV(const std::string& filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open()) {
        std::cerr << "Fatal : Unable to open shader file '" << filename << "'\n";
        std::exit(-1);
    }
    size_t size = file.tellg();
    std::vector<char> buffer(size);
    file.seekg(0);
    file.read(buffer.data(), size);
    return buffer;
}

// Create buffer + allocate + bind memory
void createBuffer(
    VkDevice device,
    VkPhysicalDevice physicalDevice,
    VkDeviceSize size,
    VkBufferUsageFlags usage,
    VkMemoryPropertyFlags props,
    VkBuffer& buffer,
    VkDeviceMemory& bufferMemory
) {
    std::cout << "Creating " << size << " bytes large storage buffer\n";
    VkBufferCreateInfo bufferInfo{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO};
    bufferInfo.size = size;
    bufferInfo.usage = usage;
    bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    VK_CHECK(vkCreateBuffer(device, &bufferInfo, nullptr, &buffer));

    VkMemoryRequirements memReq;
    vkGetBufferMemoryRequirements(device, buffer, &memReq);

    VkMemoryAllocateInfo allocInfo{VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO};
    allocInfo.allocationSize = memReq.size;

    VkPhysicalDeviceMemoryProperties memProps;
    vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProps);

    for (uint32_t i = 0; i < memProps.memoryTypeCount; i++) {
        if (memReq.memoryTypeBits & (1 << i) && (memProps.memoryTypes[i].propertyFlags & props) == props) {
            allocInfo.memoryTypeIndex = i;
            break;
        }
    }

    VK_CHECK(vkAllocateMemory(device, &allocInfo, nullptr, &bufferMemory));
    VK_CHECK(vkBindBufferMemory(device, buffer, bufferMemory, 0));
}

void destroyBuffer(VkDevice device, VkBuffer buffer, VkDeviceMemory bufferMemory) {
    std ::cout << "destroying buffer" << "\n";
    vkDestroyBuffer(device, buffer, nullptr);
    std ::cout << "freeing buffer memory" << "\n";
    vkFreeMemory(device, bufferMemory, nullptr);
}

VkInstance createInstance() {
    std::cout << "Creating instance\n";
    VkInstance instance;
    VkInstanceCreateInfo instInfo{VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
    instInfo.enabledLayerCount = static_cast<uint32_t>(validationLayers.size());
    instInfo.ppEnabledLayerNames = validationLayers.data();
    VK_CHECK(vkCreateInstance(&instInfo, nullptr, &instance));
    return instance;
}

VkPhysicalDevice getPhysicalDevice(VkInstance instance) {
    uint32_t devCount = 0;
    vkEnumeratePhysicalDevices(instance, &devCount, nullptr);
    std::vector<VkPhysicalDevice> physicalDevices(devCount);
    vkEnumeratePhysicalDevices(instance, &devCount, physicalDevices.data());

    std::cout << "Available physical devices:\n";
    for (auto const& dev : physicalDevices) {
        VkPhysicalDeviceProperties props;
        vkGetPhysicalDeviceProperties(dev, &props);
        std::cout << "- " << props.deviceName << " (type: " << vk::to_string((vk::PhysicalDeviceType)props.deviceType)
                  << ", vendor: " << vk::to_string((vk::VendorId)props.vendorID) << ")\n";
    }

    std::cout << "Using first device\n";

    return physicalDevices[0];
}

std::pair<VkDevice, uint> createLogicalDeviceAndQueue(VkPhysicalDevice physicalDevice) {
    std::cout << "Searching for compute-only queue family\n";
    uint queueFamilyCount;
    vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &queueFamilyCount, nullptr);
    std::vector<VkQueueFamilyProperties> queueFamilies(queueFamilyCount);
    vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &queueFamilyCount, queueFamilies.data());

    int foundIndex = -1;
    for (int i = 0; i < queueFamilies.size(); i++) {
        const auto& family = queueFamilies.at(i);
        if (family.queueFlags & VK_QUEUE_COMPUTE_BIT && !(family.queueFlags & VK_QUEUE_GRAPHICS_BIT)) {
            foundIndex = i;
            break;
        }
    }
    if (foundIndex == -1) {
        std::cout << "No compute-only queue family found. Falling back to index 0\n";
        foundIndex = 0;
    }

    float priority = 1.0f;
    VkDeviceQueueCreateInfo queueCreateInfo{VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO};
    queueCreateInfo.queueFamilyIndex = foundIndex;
    queueCreateInfo.queueCount = 1;
    queueCreateInfo.pQueuePriorities = &priority;

    std::cout << "Creating logical device\n";
    VkDevice device;
    VkDeviceCreateInfo deviceInfo{VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO};
    deviceInfo.queueCreateInfoCount = 1;
    deviceInfo.pQueueCreateInfos = &queueCreateInfo;
    VK_CHECK(vkCreateDevice(physicalDevice, &deviceInfo, nullptr, &device));

    return std::make_pair(device, foundIndex);
}

VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, std::vector<VkDescriptorSetLayoutBinding> const& bindings) {
    VkDescriptorSetLayoutCreateInfo layoutInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
    layoutInfo.bindingCount = 3;
    layoutInfo.pBindings = bindings.data();

    VkDescriptorSetLayout descriptorSetLayout;
    VK_CHECK(vkCreateDescriptorSetLayout(device, &layoutInfo, nullptr, &descriptorSetLayout));
    return descriptorSetLayout;
}

VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout setLayout, VkPushConstantRange pushConstants) {
    std::cout << "Creating PipelineLayout\n";
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &setLayout;
    pipelineLayoutInfo.pPushConstantRanges = &pushConstants;
    pipelineLayoutInfo.pushConstantRangeCount = 1;

    VkPipelineLayout pipelineLayout;
    VK_CHECK(vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout));
    return pipelineLayout;
}

VkPipeline loadComputeShader(VkDevice device, VkPipelineLayout pipelineLayout, std::string const& filename) {
    std::cout << "Loading ShaderModule \"" << filename << "\"\n";
    auto shaderCode = readSPV(filename);
    VkShaderModuleCreateInfo shaderModuleCreateInfo{VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO};
    shaderModuleCreateInfo.codeSize = shaderCode.size();
    shaderModuleCreateInfo.pCode = reinterpret_cast<const uint32_t*>(shaderCode.data());

    VkShaderModule shaderModule;
    VK_CHECK(vkCreateShaderModule(device, &shaderModuleCreateInfo, nullptr, &shaderModule));

    std::cout << "Creating Pipeline\n";
    VkComputePipelineCreateInfo pipelineInfo{VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO};
    pipelineInfo.stage = {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO};
    pipelineInfo.stage.stage = VK_SHADER_STAGE_COMPUTE_BIT;
    pipelineInfo.stage.module = shaderModule;
    pipelineInfo.stage.pName = "main";
    pipelineInfo.layout = pipelineLayout;


    VkPipeline pipeline;
    VK_CHECK(vkCreateComputePipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &pipeline));

    std ::cout << "destroying shader module" << "\n";
    vkDestroyShaderModule(device, shaderModule, nullptr);

    return pipeline;
}


VkDescriptorPool createDescriptorPool(VkDevice device) {
    std::cout << "Creating Descriptor Pool\n";
    VkDescriptorPoolSize poolSize{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 3};
    VkDescriptorPoolCreateInfo poolInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO};
    poolInfo.maxSets = 1;
    poolInfo.poolSizeCount = 1;
    poolInfo.pPoolSizes = &poolSize;

    VkDescriptorPool descriptorPool;
    VK_CHECK(vkCreateDescriptorPool(device, &poolInfo, nullptr, &descriptorPool));
    return descriptorPool;
}

VkDescriptorSet allocateDescriptorSet(VkDevice device, VkDescriptorSetLayout descriptorSetLayout, VkDescriptorPool descriptorPool) {
    std::cout << "Creating Descriptor Set\n";
    VkDescriptorSetAllocateInfo allocInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
    allocInfo.descriptorPool = descriptorPool;
    allocInfo.descriptorSetCount = 1;
    allocInfo.pSetLayouts = &descriptorSetLayout;

    VkDescriptorSet descriptorSet;
    VK_CHECK(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet));
    return descriptorSet;
}

VkCommandPool createCommandPool(VkDevice device, uint queueFamilyIndex) {
    std::cout << "Creating Command pool\n";
    VkCommandPoolCreateInfo poolCreateInfo{VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO};
    poolCreateInfo.queueFamilyIndex = queueFamilyIndex;
    poolCreateInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    VkCommandPool commandPool;
    VK_CHECK(vkCreateCommandPool(device, &poolCreateInfo, nullptr, &commandPool));
    return commandPool;
}
VkCommandBuffer allocateCommandBuffer(VkDevice device, VkCommandPool commandPool) {
    VkCommandBufferAllocateInfo cmdAlloc{VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO};
    cmdAlloc.commandPool = commandPool;
    cmdAlloc.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    cmdAlloc.commandBufferCount = 1;

    std::cout << "Allocating command buffer\n";
    VkCommandBuffer commandBuffer;
    VK_CHECK(vkAllocateCommandBuffers(device, &cmdAlloc, &commandBuffer));
    return commandBuffer;
}

struct VulkanSetupData {
    VkInstance instance;
    VkPhysicalDevice physicalDevice;
    VkDevice device;
    uint queueFamilyIndex;
    VkQueue queue;
};
static thread_local VulkanSetupData global_s;

void setupVulkan() {
    if (!checkValidationLayerSupport()) {
        throw std::runtime_error("validation layers requested, but not available!");
    }

    // === Vulkan instance + physical device + logical device ===
    VkInstance instance = createInstance();

    VkPhysicalDevice physicalDevice = getPhysicalDevice(instance);

    auto&& [_device, _queueFamilyIndex] = createLogicalDeviceAndQueue(physicalDevice);
    VkDevice device = _device;
    uint queueFamilyIndex = _queueFamilyIndex;

    VkQueue queue;
    vkGetDeviceQueue(device, queueFamilyIndex, 0, &queue);

    global_s = VulkanSetupData{
        .instance = instance,
        .physicalDevice = physicalDevice,
        .device = device,
        .queueFamilyIndex = queueFamilyIndex,
        .queue = queue,
    };
}

void teardownVulkan() {
    std ::cout << "destroying logical device" << "\n";
    vkDestroyDevice(global_s.device, nullptr);
    std ::cout << "destroying instance" << "\n";
    vkDestroyInstance(global_s.instance, nullptr);
}

struct CommonBuffer {
    VkDevice device;
    VkBuffer buf;
    VkDeviceMemory mem;
    VkDeviceSize size;
};

CommonBuffer createCommonBuffer(VkDeviceSize size) {
    VkBuffer buf;
    VkDeviceMemory mem;
    createBuffer(
        global_s.device,
        global_s.physicalDevice,
        size,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        buf,
        mem
    );
    return CommonBuffer{global_s.device, buf, mem, size};
}

void destroyCommonBuffer(CommonBuffer&& buf) {
    destroyBuffer(buf.device, buf.buf, buf.mem);
}

struct PerShaderData {
    VkDescriptorSetLayout descriptorSetLayout;
    VkDescriptorPool descriptorPool;
    VkDescriptorSet descriptorSet;
    VkPipelineLayout pipelineLayout;
    VkPipeline pipeline;
    VkCommandPool commandPool;
    VkCommandBuffer commandBuffer;
};

PerShaderData perShaderSetup(std::vector<CommonBuffer> buffersToBind) {
    std::cout << "Creating DescriptorSetLayouts\n";
    std::vector<VkDescriptorSetLayoutBinding> bindings;
    for (int i = 0; i < buffersToBind.size(); ++i) {
        VkDescriptorSetLayoutBinding binding{};
        binding.binding = i;
        binding.descriptorCount = 1;
        binding.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        binding.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
        bindings.push_back(binding);
    }

    VkDescriptorSetLayout descriptorSetLayout = createDescriptorSetLayout(global_s.device, bindings);

    VkDescriptorPool descriptorPool = createDescriptorPool(global_s.device);

    VkDescriptorSet descriptorSet = allocateDescriptorSet(global_s.device, descriptorSetLayout, descriptorPool);

    // === Pipeline Layout ===
    VkPushConstantRange pushConstantRange{};
    pushConstantRange.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
    pushConstantRange.offset = 0;
    pushConstantRange.size = 4;

    VkPipelineLayout pipelineLayout = createPipelineLayout(global_s.device, descriptorSetLayout, pushConstantRange);

    VkPipeline pipeline = loadComputeShader(global_s.device, pipelineLayout, "compute.comp.spv");

    std::vector<VkDescriptorBufferInfo> infos;
    for (auto const& buffer : buffersToBind) {
        infos.push_back(VkDescriptorBufferInfo{buffer.buf, 0, buffer.size});
    }

    std::vector<VkWriteDescriptorSet> writes;
    for (int i = 0; i < infos.size(); ++i) {
        VkWriteDescriptorSet wset{VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET};
        wset.dstSet = descriptorSet;
        wset.dstBinding = i;
        wset.descriptorCount = 1;
        wset.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        wset.pBufferInfo = &infos[i];
        writes.push_back(wset);
    }

    vkUpdateDescriptorSets(global_s.device, writes.size(), writes.data(), 0, nullptr);

    VkCommandPool commandPool = createCommandPool(global_s.device, global_s.queueFamilyIndex);
    VkCommandBuffer commandBuffer = allocateCommandBuffer(global_s.device, commandPool);


    return PerShaderData{
        .descriptorSetLayout = descriptorSetLayout,
        .descriptorPool = descriptorPool,
        .descriptorSet = descriptorSet,
        .pipelineLayout = pipelineLayout,
        .pipeline = pipeline,
        .commandPool = commandPool,
        .commandBuffer = commandBuffer,
    };
}

void perShaderTeardown(PerShaderData&& sd) {
    std ::cout << "destroying pipeline" << "\n";
    vkDestroyPipeline(global_s.device, sd.pipeline, nullptr);
    std ::cout << "destroying command pool" << "\n";
    vkDestroyCommandPool(global_s.device, sd.commandPool, nullptr);
    std ::cout << "destroying pipeline_layout" << "\n";
    vkDestroyPipelineLayout(global_s.device, sd.pipelineLayout, nullptr);
    std ::cout << "destroying descriptor pool" << "\n";
    vkDestroyDescriptorPool(global_s.device, sd.descriptorPool, nullptr);
    std ::cout << "destroying descriptor set" << "\n";
    vkDestroyDescriptorSetLayout(global_s.device, sd.descriptorSetLayout, nullptr);
}

void runShader(PerShaderData const& sd, uint computeSize) {
    std::cout << "Filling command buffer\n";
    VkCommandBufferBeginInfo beginInfo{VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
    vkBeginCommandBuffer(sd.commandBuffer, &beginInfo);
    vkCmdBindPipeline(sd.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, sd.pipeline);
    vkCmdBindDescriptorSets(sd.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, sd.pipelineLayout, 0, 1, &sd.descriptorSet, 0, nullptr);
    vkCmdPushConstants(sd.commandBuffer, sd.pipelineLayout, VK_SHADER_STAGE_COMPUTE_BIT, 0, 4, &computeSize);
    vkCmdDispatch(sd.commandBuffer, (computeSize + 1023) / 1024, 1, 1);
    vkEndCommandBuffer(sd.commandBuffer);

    std::cout << "Submitting command buffer\n";
    VkSubmitInfo submitInfo{VK_STRUCTURE_TYPE_SUBMIT_INFO};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &sd.commandBuffer;
    VK_CHECK(vkQueueSubmit(global_s.queue, 1, &submitInfo, VK_NULL_HANDLE));
    std::cout << "Waiting for Device to finish computation\n";
    VK_CHECK(vkQueueWaitIdle(global_s.queue));
}


int main() {
    setupVulkan();

    // ----- Fleet -----
    std::vector<int> A = {1, 2, 3};
    std::vector<int> B = {4, 5, 6};
    std::vector<int> C(3);
    // ----- Fleet -----

    std::cout << "Allocating buffers A, B and C on GPU\n";
    VkDeviceSize bufferSize = A.size() * sizeof(int);

    // fl_runtime_allocate_gpu_backing(&fleet_a, sizeof(fleet_a))
    auto bufA = createCommonBuffer(bufferSize);
    // fl_runtime_allocate_gpu_backing(&fleet_b, sizeof(fleet_b))
    auto bufB = createCommonBuffer(bufferSize);
    // fl_runtime_allocate_gpu_backing(&fleet_c, sizeof(fleet_c))
    auto bufC = createCommonBuffer(bufferSize);


    std::cout << "Copying Host -> Device\n";
    void* data;
    // fl_runtime_copy_to_backing(&fleet_a)
    vkMapMemory(global_s.device, bufA.mem, 0, bufferSize, 0, &data);
    memcpy(data, A.data(), bufferSize);
    vkUnmapMemory(global_s.device, bufA.mem);

    // fl_runtime_copy_to_backing(&fleet_b)
    vkMapMemory(global_s.device, bufB.mem, 0, bufferSize, 0, &data);
    memcpy(data, B.data(), bufferSize);
    vkUnmapMemory(global_s.device, bufB.mem);

    // void *buffers[3] = {&fleet_a, &fleet_b, &fleet_c}
    // fl_runtime_bind_buffers(&buffers, 3)
    auto shaderData = perShaderSetup({bufA, bufB, bufC});

    // fl_runtime_dispatch_shader()
    runShader(shaderData, C.size());

    perShaderTeardown(std::move(shaderData));

    std::cout << "Copying Device -> Host\n";
    // fl_runtime_copy_from_backing(&fleet_c)
    vkMapMemory(global_s.device, bufC.mem, 0, bufferSize, 0, &data);
    memcpy(C.data(), data, bufferSize);
    vkUnmapMemory(global_s.device, bufC.mem);

    for (int v : C) {
        std::cout << v << " ";
    }
    std::cout << "\n";

    // fl_runtime_free_gpu_backing(&fleet_a)
    destroyCommonBuffer(std::move(bufA));
    // fl_runtime_free_gpu_backing(&fleet_a)
    destroyCommonBuffer(std::move(bufB));
    // fl_runtime_free_gpu_backing(&fleet_a)
    destroyCommonBuffer(std::move(bufC));

    teardownVulkan();
}
