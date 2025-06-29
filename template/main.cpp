#include <cstring>
#include <vulkan/vulkan.hpp>
#include <iostream>
#include <vector>
#include <fstream>
#include <cassert>
#include <vulkan/vulkan_core.h>

#include <functional>
#include <vulkan/vulkan_to_string.hpp>


class ScopeGuard {
public:
    explicit ScopeGuard(std::function<void()> f): dtor(std::move(f)), active(true) {}
    ~ScopeGuard() {
        if (active)
            dtor();
    }

    ScopeGuard(const ScopeGuard&) = delete;
    ScopeGuard& operator=(const ScopeGuard&) = delete;

    ScopeGuard(ScopeGuard&& other) noexcept: dtor(std::move(other.dtor)), active(other.active) {
        other.active = false;
    }

    ScopeGuard& operator=(ScopeGuard&& other) noexcept {
        if (this != &other) {
            dtor = std::move(other.dtor);
            active = other.active;
            other.active = false;
        }
        return *this;
    }

    static ScopeGuard merge(ScopeGuard&& a, ScopeGuard&& b) noexcept {
        auto a_dtor = a.dtor;
        auto b_dtor = b.dtor;

        a.active = false;
        b.active = false;

        return ScopeGuard([a_dtor, b_dtor]() {
            a_dtor();
            b_dtor();
        });
    }

private:
    std::function<void()> dtor;
    bool active;
};

#define EXPAND(x) x
#define CONCAT_impl(x, y) x##y
#define CONCAT(x, y) CONCAT_impl(x, y)

#define DEFER_VALUE(...) ScopeGuard([&]() { __VA_ARGS__; })
#define DEFER(...) auto CONCAT(_defer_guard_, __LINE__) = DEFER_VALUE(__VA_ARGS__)


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
ScopeGuard createBuffer(
    VkDevice device,
    VkPhysicalDevice physicalDevice,
    VkDeviceSize size,
    VkBufferUsageFlags usage,
    VkMemoryPropertyFlags props,
    VkBuffer& buffer,
    VkDeviceMemory& bufferMemory
) {
    VkBufferCreateInfo bufferInfo{VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO};
    bufferInfo.size = size;
    bufferInfo.usage = usage;
    bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    VK_CHECK(vkCreateBuffer(device, &bufferInfo, nullptr, &buffer));
    auto buffer_dtor = ScopeGuard([device, buffer]() { vkDestroyBuffer(device, buffer, nullptr); });

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
    auto memory_dtor = ScopeGuard([device, bufferMemory]() { vkFreeMemory(device, bufferMemory, nullptr); });
    VK_CHECK(vkBindBufferMemory(device, buffer, bufferMemory, 0));

    return ScopeGuard::merge(std::move(memory_dtor), std::move(buffer_dtor));
}

int main() {
    if (!checkValidationLayerSupport()) {
        throw std::runtime_error("validation layers requested, but not available!");
    }

    // === Vulkan instance + physical device + logical device ===
    std::cout << "Creating instance\n";
    VkInstance instance;
    VkInstanceCreateInfo instInfo{VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
    instInfo.enabledLayerCount = static_cast<uint32_t>(validationLayers.size());
    instInfo.ppEnabledLayerNames = validationLayers.data();
    VK_CHECK(vkCreateInstance(&instInfo, nullptr, &instance));
    DEFER(vkDestroyInstance(instance, nullptr));

    uint32_t devCount = 0;
    vkEnumeratePhysicalDevices(instance, &devCount, nullptr);
    std::vector<VkPhysicalDevice> physicalDevices(devCount);
    vkEnumeratePhysicalDevices(instance, &devCount, physicalDevices.data());

    VkPhysicalDevice physicalDevice = physicalDevices[0];

    float priority = 1.0f;
    std::cout << "Creating compute queue\n";
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
    DEFER(vkDestroyDevice(device, nullptr));


    VkQueue queue;
    vkGetDeviceQueue(device, foundIndex, 0, &queue);

    // === Buffers ===
    std::vector<int> A = {1, 2, 3};
    std::vector<int> B = {4, 5, 6};
    std::vector<int> C(3);

    std::cout << "Allocating 3x " << A.size() * sizeof(int) << " bytes on the GPU\n";

    VkDeviceSize bufferSize = A.size() * sizeof(int);

    VkBuffer bufA, bufB, bufC;
    VkDeviceMemory memA, memB, memC;
    auto bufA_dtor = createBuffer(
        device,
        physicalDevice,
        bufferSize,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        bufA,
        memA
    );
    auto bufB_dtor = createBuffer(
        device,
        physicalDevice,
        bufferSize,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        bufB,
        memB
    );
    auto bufC_dtor = createBuffer(
        device,
        physicalDevice,
        bufferSize,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        bufC,
        memC
    );


    std::cout << "Copying Host -> Device\n";
    void* data;
    vkMapMemory(device, memA, 0, bufferSize, 0, &data);
    memcpy(data, A.data(), bufferSize);
    vkUnmapMemory(device, memA);

    vkMapMemory(device, memB, 0, bufferSize, 0, &data);
    memcpy(data, B.data(), bufferSize);
    vkUnmapMemory(device, memB);

    // === Descriptor Set Layout ===
    std::cout << "Creating DescriptorSetLayouts\n";
    VkDescriptorSetLayoutBinding bindings[3];
    for (int i = 0; i < 3; ++i) {
        bindings[i] = {};
        bindings[i].binding = i;
        bindings[i].descriptorCount = 1;
        bindings[i].descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        bindings[i].stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
    }

    VkDescriptorSetLayoutCreateInfo layoutInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
    layoutInfo.bindingCount = 3;
    layoutInfo.pBindings = bindings;

    VkDescriptorSetLayout descriptorSetLayout;
    VK_CHECK(vkCreateDescriptorSetLayout(device, &layoutInfo, nullptr, &descriptorSetLayout));
    DEFER(vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr));

    // === Pipeline Layout ===
    std::cout << "Creating PipelineLayout\n";
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &descriptorSetLayout;


    VkPushConstantRange pushConstantRange{};
    pushConstantRange.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
    pushConstantRange.offset = 0;
    pushConstantRange.size = 4;
    pipelineLayoutInfo.pPushConstantRanges = &pushConstantRange;
    pipelineLayoutInfo.pushConstantRangeCount = 1;

    VkPipelineLayout pipelineLayout;
    VK_CHECK(vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout));
    DEFER(vkDestroyPipelineLayout(device, pipelineLayout, nullptr));

    // === Shader Module ===
    std::cout << "Loading ShaderModule\n";
    auto shaderCode = readSPV("compute.comp.spv");
    VkShaderModuleCreateInfo shaderModuleCreateInfo{VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO};
    shaderModuleCreateInfo.codeSize = shaderCode.size();
    shaderModuleCreateInfo.pCode = reinterpret_cast<const uint32_t*>(shaderCode.data());

    VkShaderModule shaderModule;
    VK_CHECK(vkCreateShaderModule(device, &shaderModuleCreateInfo, nullptr, &shaderModule));
    DEFER(vkDestroyShaderModule(device, shaderModule, nullptr));

    // === Pipeline ===
    std::cout << "Creating Pipeline\n";
    VkComputePipelineCreateInfo pipelineInfo{VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO};
    pipelineInfo.stage = {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO};
    pipelineInfo.stage.stage = VK_SHADER_STAGE_COMPUTE_BIT;
    pipelineInfo.stage.module = shaderModule;
    pipelineInfo.stage.pName = "main";
    pipelineInfo.layout = pipelineLayout;


    VkPipeline pipeline;
    VK_CHECK(vkCreateComputePipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &pipeline));
    DEFER(vkDestroyPipeline(device, pipeline, nullptr));

    // === Descriptor Pool and Set ===
    std::cout << "Creating Descriptor Pool\n";
    VkDescriptorPoolSize poolSize{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 3};
    VkDescriptorPoolCreateInfo poolInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO};
    poolInfo.maxSets = 1;
    poolInfo.poolSizeCount = 1;
    poolInfo.pPoolSizes = &poolSize;

    VkDescriptorPool descriptorPool;
    VK_CHECK(vkCreateDescriptorPool(device, &poolInfo, nullptr, &descriptorPool));
    DEFER(vkDestroyDescriptorPool(device, descriptorPool, nullptr));

    std::cout << "Creating Descriptor Set\n";
    VkDescriptorSetAllocateInfo allocInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
    allocInfo.descriptorPool = descriptorPool;
    allocInfo.descriptorSetCount = 1;
    allocInfo.pSetLayouts = &descriptorSetLayout;

    VkDescriptorSet descriptorSet;
    VK_CHECK(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet));
    // not needed because descriptor sets are bound to their pool
    // DEFER(vkFreeDescriptorSets(device, descriptorPool, 1, &descriptorSet));

    VkDescriptorBufferInfo infos[3] = {
        {bufA, 0, bufferSize},
        {bufB, 0, bufferSize},
        {bufC, 0, bufferSize}
    };

    VkWriteDescriptorSet writes[3];
    for (int i = 0; i < 3; ++i) {
        writes[i] = {VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET};
        writes[i].dstSet = descriptorSet;
        writes[i].dstBinding = i;
        writes[i].descriptorCount = 1;
        writes[i].descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        writes[i].pBufferInfo = &infos[i];
    }

    vkUpdateDescriptorSets(device, 3, writes, 0, nullptr);

    // === Command Buffer ===
    std::cout << "Creating Command pool\n";
    VkCommandPoolCreateInfo poolCreateInfo{VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO};
    poolCreateInfo.queueFamilyIndex = foundIndex;
    poolCreateInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    VkCommandPool commandPool;
    VK_CHECK(vkCreateCommandPool(device, &poolCreateInfo, nullptr, &commandPool));
    DEFER(vkDestroyCommandPool(device, commandPool, nullptr));

    VkCommandBufferAllocateInfo cmdAlloc{VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO};
    cmdAlloc.commandPool = commandPool;
    cmdAlloc.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    cmdAlloc.commandBufferCount = 1;

    std::cout << "Allocating command buffer\n";
    VkCommandBuffer commandBuffer;
    VK_CHECK(vkAllocateCommandBuffers(device, &cmdAlloc, &commandBuffer));

    std::cout << "Filling command buffer\n";
    VkCommandBufferBeginInfo beginInfo{VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
    vkBeginCommandBuffer(commandBuffer, &beginInfo);
    vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
    int computeSize = A.size();
    vkCmdPushConstants(commandBuffer, pipelineLayout, VK_SHADER_STAGE_COMPUTE_BIT, 0, 4, &computeSize);
    vkCmdDispatch(commandBuffer, (computeSize + 1023) / 1024, 1, 1);
    vkEndCommandBuffer(commandBuffer);

    std::cout << "Submitting command buffer\n";
    VkSubmitInfo submitInfo{VK_STRUCTURE_TYPE_SUBMIT_INFO};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &commandBuffer;
    VK_CHECK(vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE));
    std::cout << "Waiting for Device to finish computation\n";
    VK_CHECK(vkQueueWaitIdle(queue));

    // === Read back results ===
    std::cout << "Copying Device -> Host\n";
    vkMapMemory(device, memC, 0, bufferSize, 0, &data);
    memcpy(C.data(), data, bufferSize);
    vkUnmapMemory(device, memC);

    for (int v : C) {
        std::cout << v << " ";
    }
    std::cout << "\n";
}
