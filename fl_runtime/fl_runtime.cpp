#include <cstdlib>
#include <cstring>
#include <stdexcept>
#include <unordered_map>
#include <utility>
#include <vulkan/vulkan.hpp>
#include <iostream>
#include <vector>
#include <cassert>
#include <fstream>

#include <vulkan/vulkan_core.h>
#include <vulkan/vulkan_enums.hpp>
#include <vulkan/vulkan_handles.hpp>
#include <vulkan/vulkan_structs.hpp>
#include <vulkan/vulkan_to_string.hpp>

#include "fl_runtime.h"


#define VK_CHECK(x)                                                                                         \
    do {                                                                                                    \
        VkResult err = x;                                                                                   \
        if (err) {                                                                                          \
            std::cerr << "Fatal : VkResult is \"" << vk::to_string((vk::Result)err) << "\" at " << __FILE__ \
                      << ":" << __LINE__ << "\n";                                                           \
            std::exit(-1);                                                                                  \
        }                                                                                                   \
    } while (0)


VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(
    VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType,
    const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData,
    void* pUserData
) {

    std::cout << "Validation layer:\n" << pCallbackData->pMessage << "\n";
    return VK_FALSE;
}

// #define DO_LOG

#ifdef DO_LOG
    #define LOG(X) X
#else
    #define LOG(X)
#endif

struct VulkanSetupData {
    VkInstance instance;
    VkDebugUtilsMessengerEXT debugMessenger;
    VkPhysicalDevice physicalDevice;
    VkDevice device;
    uint queueFamilyIndex;
    VkQueue queue;
};

static thread_local VulkanSetupData global_s;

struct CommonBuffer {
    VkDevice device;
    VkBuffer buf;
    VkDeviceMemory mem;
    VkDeviceSize size;
};

static const std::vector<const char*> validationLayers = {
    "VK_LAYER_KHRONOS_validation",
};
static const std::vector<const char*> extensions = {
    VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
    VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME,
};


static bool checkValidationLayerSupport() {
    uint32_t layerCount;
    vkEnumerateInstanceLayerProperties(&layerCount, nullptr);

    std::vector<VkLayerProperties> availableLayers(layerCount);
    vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.data());

    LOG(std::cerr << "Available validation layers:\n");
    for (const auto& layerProperties : availableLayers) {
        LOG(std::cerr << "- " << layerProperties.layerName << "\n");
    }

    LOG(std::cerr << "Requested validation layers:\n");
    for (const auto& layerName : validationLayers) {
        LOG(std::cerr << "- " << layerName << "\n");
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

static VkDebugUtilsMessengerEXT createDebugMessenger(VkInstance instance) {
    VkDebugUtilsMessengerEXT debugMessenger;

    VkDebugUtilsMessengerCreateInfoEXT debugCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        .messageSeverity = /*VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT | */ VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                         | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                     | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        .pfnUserCallback = debugCallback,
        .pUserData = NULL
    };

    auto CreateDebugUtilsMessengerEXT = (PFN_vkCreateDebugUtilsMessengerEXT
    )vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");

    VK_CHECK(CreateDebugUtilsMessengerEXT(instance, &debugCreateInfo, NULL, &debugMessenger));

    return debugMessenger;
}

static void createBuffer(
    VkDevice device,
    VkPhysicalDevice physicalDevice,
    VkDeviceSize size,
    VkBufferUsageFlags usage,
    VkMemoryPropertyFlags props,
    VkBuffer& buffer,
    VkDeviceMemory& bufferMemory
) {
    LOG(std::cerr << "Creating " << size << " bytes large storage buffer\n");
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

static void destroyBuffer(VkDevice device, VkBuffer buffer, VkDeviceMemory bufferMemory) {
    LOG(std::cerr << "destroying buffer" << "\n");
    vkDestroyBuffer(device, buffer, nullptr);
    LOG(std::cerr << "freeing buffer memory" << "\n");
    vkFreeMemory(device, bufferMemory, nullptr);
}

static VkInstance createInstance() {
    LOG(std::cerr << "Creating instance\n");

    VkApplicationInfo appInfo{VK_STRUCTURE_TYPE_APPLICATION_INFO};
    appInfo.pApplicationName = "Fleet Runtime";
    appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
    appInfo.pEngineName = "Fleet Runtime";
    appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
    appInfo.apiVersion = VK_API_VERSION_1_3;

    VkInstanceCreateInfo instInfo{VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
    instInfo.pApplicationInfo = &appInfo;

    instInfo.enabledLayerCount = static_cast<uint32_t>(validationLayers.size());
    instInfo.ppEnabledLayerNames = validationLayers.data();

    instInfo.enabledExtensionCount = static_cast<uint32_t>(extensions.size());
    instInfo.ppEnabledExtensionNames = extensions.data();

    VkInstance instance;
    VK_CHECK(vkCreateInstance(&instInfo, nullptr, &instance));
    return instance;
}

static VkPhysicalDevice getPhysicalDevice(VkInstance instance) {
    uint32_t devCount = 0;
    vkEnumeratePhysicalDevices(instance, &devCount, nullptr);
    std::vector<VkPhysicalDevice> physicalDevices(devCount);
    vkEnumeratePhysicalDevices(instance, &devCount, physicalDevices.data());

    LOG(std::cerr << "Available physical devices:\n");
    for (auto const& dev : physicalDevices) {
        VkPhysicalDeviceProperties props;
        vkGetPhysicalDeviceProperties(dev, &props);
        LOG(std::cerr << "- " << props.deviceName << " (type: " << vk::to_string((vk::PhysicalDeviceType)props.deviceType)
                      << ", vendor: " << vk::to_string((vk::VendorId)props.vendorID) << ")\n");
    }

    LOG(std::cerr << "Using first device\n");

    VkPhysicalDeviceProperties props;
    vkGetPhysicalDeviceProperties(physicalDevices[0], &props);
    std::cout << "maxComputeWorkGroupSize: " << props.limits.maxComputeWorkGroupSize[0] << ", "
              << props.limits.maxComputeWorkGroupSize[1] << ", " << props.limits.maxComputeWorkGroupSize[2] << "\n";


    return physicalDevices[0];
}

static std::pair<VkDevice, uint> createLogicalDeviceAndQueue(VkPhysicalDevice physicalDevice) {
    LOG(std::cerr << "Searching for compute-only queue family\n");
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
        LOG(std::cerr << "No compute-only queue family found. Falling back to index 0\n");
        foundIndex = 0;
    }

    float priority = 1.0f;
    VkDeviceQueueCreateInfo queueCreateInfo{VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO};
    queueCreateInfo.queueFamilyIndex = foundIndex;
    queueCreateInfo.queueCount = 1;
    queueCreateInfo.pQueuePriorities = &priority;

    LOG(std::cerr << "Creating logical device\n");
    VkDevice device;
    VkDeviceCreateInfo deviceInfo{VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO};
    deviceInfo.queueCreateInfoCount = 1;
    deviceInfo.pQueueCreateInfos = &queueCreateInfo;
    VK_CHECK(vkCreateDevice(physicalDevice, &deviceInfo, nullptr, &device));

    return std::make_pair(device, foundIndex);
}

static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, std::vector<VkDescriptorSetLayoutBinding> const& bindings) {
    VkDescriptorSetLayoutCreateInfo layoutInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
    layoutInfo.bindingCount = bindings.size();
    layoutInfo.pBindings = bindings.data();

    VkDescriptorSetLayout descriptorSetLayout;
    VK_CHECK(vkCreateDescriptorSetLayout(device, &layoutInfo, nullptr, &descriptorSetLayout));
    return descriptorSetLayout;
}

static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout setLayout, VkPushConstantRange pushConstants) {
    LOG(std::cerr << "Creating PipelineLayout\n");
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &setLayout;
    pipelineLayoutInfo.pPushConstantRanges = &pushConstants;
    pipelineLayoutInfo.pushConstantRangeCount = 1;

    VkPipelineLayout pipelineLayout;
    VK_CHECK(vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout));
    return pipelineLayout;
}

static VkPipeline loadComputeShader(VkDevice device, VkPipelineLayout pipelineLayout, const uint32_t* code, uint64_t code_size) {
    VkShaderModuleCreateInfo shaderModuleCreateInfo{VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO};
    shaderModuleCreateInfo.codeSize = code_size;
    shaderModuleCreateInfo.pCode = code;

    VkShaderModule shaderModule;
    VK_CHECK(vkCreateShaderModule(device, &shaderModuleCreateInfo, nullptr, &shaderModule));

    LOG(std::cerr << "Creating Pipeline\n");
    VkComputePipelineCreateInfo pipelineInfo{VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO};
    pipelineInfo.stage = {VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO};
    pipelineInfo.stage.stage = VK_SHADER_STAGE_COMPUTE_BIT;
    pipelineInfo.stage.module = shaderModule;
    pipelineInfo.stage.pName = "main";
    pipelineInfo.layout = pipelineLayout;


    VkPipeline pipeline;
    VK_CHECK(vkCreateComputePipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &pipeline));

    LOG(std::cerr << "destroying shader module" << "\n");
    vkDestroyShaderModule(device, shaderModule, nullptr);

    return pipeline;
}


static VkDescriptorPool createDescriptorPool(VkDevice device, uint32_t numBuffers) {
    LOG(std::cerr << "Creating Descriptor Pool\n");
    VkDescriptorPoolSize poolSize{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, numBuffers};
    VkDescriptorPoolCreateInfo poolInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO};
    poolInfo.maxSets = 1;
    poolInfo.poolSizeCount = 1;
    poolInfo.pPoolSizes = &poolSize;

    VkDescriptorPool descriptorPool;
    VK_CHECK(vkCreateDescriptorPool(device, &poolInfo, nullptr, &descriptorPool));
    return descriptorPool;
}

static VkDescriptorSet allocateDescriptorSet(VkDevice device, VkDescriptorSetLayout descriptorSetLayout, VkDescriptorPool descriptorPool) {
    LOG(std::cerr << "Creating Descriptor Set\n");
    VkDescriptorSetAllocateInfo allocInfo{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
    allocInfo.descriptorPool = descriptorPool;
    allocInfo.descriptorSetCount = 1;
    allocInfo.pSetLayouts = &descriptorSetLayout;

    VkDescriptorSet descriptorSet;
    VK_CHECK(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet));
    return descriptorSet;
}

static VkCommandPool createCommandPool(VkDevice device, uint queueFamilyIndex) {
    LOG(std::cerr << "Creating Command pool\n");
    VkCommandPoolCreateInfo poolCreateInfo{VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO};
    poolCreateInfo.queueFamilyIndex = queueFamilyIndex;
    poolCreateInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    VkCommandPool commandPool;
    VK_CHECK(vkCreateCommandPool(device, &poolCreateInfo, nullptr, &commandPool));
    return commandPool;
}
static VkCommandBuffer allocateCommandBuffer(VkDevice device, VkCommandPool commandPool) {
    VkCommandBufferAllocateInfo cmdAlloc{VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO};
    cmdAlloc.commandPool = commandPool;
    cmdAlloc.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    cmdAlloc.commandBufferCount = 1;

    LOG(std::cerr << "Allocating command buffer\n");
    VkCommandBuffer commandBuffer;
    VK_CHECK(vkAllocateCommandBuffers(device, &cmdAlloc, &commandBuffer));
    return commandBuffer;
}


static CommonBuffer createCommonBuffer(VkDeviceSize size) {
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

static void destroyCommonBuffer(CommonBuffer&& buf) {
    destroyBuffer(buf.device, buf.buf, buf.mem);
}

struct CachedShader {
    VkPipelineLayout pipelineLayout;
    VkPipeline pipeline;
    VkDescriptorSetLayout descriptorSetLayout;
};

struct PerShaderData {
    CachedShader shader;
    VkDescriptorPool descriptorPool;
    VkDescriptorSet descriptorSet;
    VkCommandPool commandPool;
    VkCommandBuffer commandBuffer;
};

static size_t hashSPIRV(const uint32_t* code, size_t size_bytes) {
    std::hash<std::string_view> hasher;
    return hasher(std::string_view(reinterpret_cast<const char*>(code), size_bytes));
}


static thread_local std::unordered_map<size_t, CachedShader> shader_cache;

static PerShaderData perShaderSetup(std::vector<CommonBuffer> buffersToBind, const uint32_t* code, uint64_t code_size) {
    size_t shaderHash = hashSPIRV(code, code_size);

    CachedShader* cached = nullptr;

    auto it = shader_cache.find(shaderHash);
    if (it == shader_cache.end()) {
        LOG(std::cerr << "Shader not found in cache. Compiling and caching...\n");

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

        VkPushConstantRange pushConstantRange{};
        pushConstantRange.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
        pushConstantRange.offset = 0;
        pushConstantRange.size = 4;

        VkPipelineLayout pipelineLayout = createPipelineLayout(global_s.device, descriptorSetLayout, pushConstantRange);

        VkPipeline pipeline = loadComputeShader(global_s.device, pipelineLayout, code, code_size);

        shader_cache[shaderHash] = CachedShader{
            .pipelineLayout = pipelineLayout,
            .pipeline = pipeline,
            .descriptorSetLayout = descriptorSetLayout,
        };

        cached = &shader_cache[shaderHash];
    }
    else {
        LOG(std::cerr << "Shader found in cache. Reusing pipeline.\n");
        cached = &it->second;
    }

    VkDescriptorPool descriptorPool = createDescriptorPool(global_s.device, buffersToBind.size());
    VkDescriptorSet descriptorSet = allocateDescriptorSet(global_s.device, cached->descriptorSetLayout, descriptorPool);

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
        .shader = *cached,
        .descriptorPool = descriptorPool,
        .descriptorSet = descriptorSet,
        .commandPool = commandPool,
        .commandBuffer = commandBuffer,
    };
}


static void perShaderTeardown(PerShaderData&& sd) {
    LOG(std::cerr << "destroying command pool" << "\n");
    vkDestroyCommandPool(global_s.device, sd.commandPool, nullptr);
    LOG(std::cerr << "destroying descriptor pool" << "\n");
    vkDestroyDescriptorPool(global_s.device, sd.descriptorPool, nullptr);
}

static void runShader(PerShaderData const& sd, uint computeSize) {
    LOG(std::cerr << "Filling command buffer\n");
    VkCommandBufferBeginInfo beginInfo{VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
    vkBeginCommandBuffer(sd.commandBuffer, &beginInfo);
    vkCmdBindPipeline(sd.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, sd.shader.pipeline);
    vkCmdBindDescriptorSets(sd.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, sd.shader.pipelineLayout, 0, 1, &sd.descriptorSet, 0, nullptr);
    vkCmdPushConstants(sd.commandBuffer, sd.shader.pipelineLayout, VK_SHADER_STAGE_COMPUTE_BIT, 0, 4, &computeSize);
    vkCmdDispatch(sd.commandBuffer, (computeSize + 1023) / 1024, 1, 1);
    vkEndCommandBuffer(sd.commandBuffer);

    LOG(std::cerr << "Submitting command buffer\n");
    VkSubmitInfo submitInfo{VK_STRUCTURE_TYPE_SUBMIT_INFO};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &sd.commandBuffer;
    VK_CHECK(vkQueueSubmit(global_s.queue, 1, &submitInfo, VK_NULL_HANDLE));
    LOG(std::cerr << "Waiting for Device to finish computation\n");
    VK_CHECK(vkQueueWaitIdle(global_s.queue));
}

static thread_local std::unordered_map<void*, CommonBuffer> backing_registry;
static thread_local std::vector<void*> bound_buffers;

extern "C" {
void fl_runtime_init(void) {
    if (!checkValidationLayerSupport()) {
        throw std::runtime_error("validation layers requested, but not available!");
    }

    VkInstance instance = createInstance();

    VkDebugUtilsMessengerEXT debugMessenger = createDebugMessenger(instance);

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

void fl_runtime_deinit(void) {
    LOG(std::cerr << "clearing shader cache" << "\n");
    for (auto& [_, cached] : shader_cache) {
        LOG(std::cerr << "destroying pipeline" << "\n");
        vkDestroyPipeline(global_s.device, cached.pipeline, nullptr);
        LOG(std::cerr << "destroying pipeline_layout" << "\n");
        vkDestroyPipelineLayout(global_s.device, cached.pipelineLayout, nullptr);
        LOG(std::cerr << "destroying descriptor set" << "\n");
        vkDestroyDescriptorSetLayout(global_s.device, cached.descriptorSetLayout, nullptr);
    }
    shader_cache.clear();

    LOG(std::cerr << "destroying logical device" << "\n");
    vkDestroyDevice(global_s.device, nullptr);

    LOG(std::cerr << "destroying debug messenger" << "\n");
    auto DestroyDebugUtilsMessengerEXT = (PFN_vkDestroyDebugUtilsMessengerEXT
    )vkGetInstanceProcAddr(global_s.instance, "vkDestroyDebugUtilsMessengerEXT");
    DestroyDebugUtilsMessengerEXT(global_s.instance, global_s.debugMessenger, NULL);

    LOG(std::cerr << "destroying instance" << "\n");
    vkDestroyInstance(global_s.instance, nullptr);
}

void fl_runtime_allocate_gpu_backing(void* buffer, uint64_t size) {
    auto backing = createCommonBuffer(size);
    if (backing_registry.contains(buffer)) {
        throw std::runtime_error("Tried allocating a backing, but the backing already exists");
    }
    backing_registry.insert({buffer, backing});
}
void fl_runtime_free_gpu_backing(void* buffer) {
    if (!backing_registry.contains(buffer)) {
        throw std::runtime_error("Tried freeing a non-existent (or already freed) backing");
    }
    CommonBuffer backing = backing_registry.extract(buffer).mapped();
    destroyCommonBuffer(std::move(backing));
}


void fl_runtime_copy_to_backing(void* buffer) {
    if (!backing_registry.contains(buffer)) {
        throw std::runtime_error("Tried copying to non-existent backing");
    }
    auto const& backing = backing_registry.at(buffer);

    void* mapped_data;
    vkMapMemory(global_s.device, backing.mem, 0, backing.size, 0, &mapped_data);
    memcpy(mapped_data, buffer, backing.size);
    vkUnmapMemory(global_s.device, backing.mem);
}

void fl_runtime_copy_from_backing(void* buffer) {
    if (!backing_registry.contains(buffer)) {
        throw std::runtime_error("Tried copying from non-existent backing");
    }
    auto const& backing = backing_registry.at(buffer);

    void* mapped_data;
    vkMapMemory(global_s.device, backing.mem, 0, backing.size, 0, &mapped_data);
    memcpy(buffer, mapped_data, backing.size);
    vkUnmapMemory(global_s.device, backing.mem);
}

void fl_runtime_bind_buffers(void* (*buffers)[], uint64_t size) {
    bound_buffers.resize(size);
    memcpy(bound_buffers.data(), buffers, size * sizeof(void*));
}

void fl_runtime_dispatch_shader(uint64_t shader_dispatch_size, const uint32_t* shader_code, uint64_t shader_code_size) {
    std::vector<CommonBuffer> backings;
    backings.reserve(bound_buffers.size());
    for (void* bound_buffer : bound_buffers) {
        if (!backing_registry.contains(bound_buffer)) {
            throw std::runtime_error("Tried dispatching shader for buffer without backing");
        }
        backings.push_back(backing_registry.at(bound_buffer));
    }

    PerShaderData sd = perShaderSetup(backings, shader_code, shader_code_size);
    runShader(sd, shader_dispatch_size);
    perShaderTeardown(std::move(sd));

    bound_buffers.clear();
}
}

static std::vector<char> readSPV(const std::string& filename) {
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
