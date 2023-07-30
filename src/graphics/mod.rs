// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is free software: you can redistribute it and / or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// STARK is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with STARK (in the LICENSE file). If not, see
// <https://www.gnu.org/licenses/>.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/graphics/mod.rs

// Graphics rendering system for STARK. Relies on ash for low level
// access to the Vulkan API. Contains code for a dedicated rendering
// thread.

// <>

use crate::sail::{self, SlHndl};
use crate::FrameHandles;

use ash::vk;

use std::mem::size_of;

mod text;

/// Sail interpreter loop for the render thread (holds graphics state)
pub fn render_loop(
    name: &'static str,
    size: [u32; 2],
    frame: FrameHandles,
    sl_reg: usize,
    sl_tbl: SlHndl,
    sl_ctr: SlHndl,
    sl_env: SlHndl,
) {
    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    let mut engine = Engine::new(frame, name, size[0], size[1]);

    let mut test_glyph = text::load();

    for _ in 0..test_glyph.len() {
        engine.colors.push([0.0, 0.0, 0.0]);
    }

    engine.lines.append(&mut test_glyph);

    let eng_hdl =
        unsafe { SlHndl::from_raw_unchecked(sail::memmgt::alloc(sl_reg, 8, sail::T_ENG_HDL_ID.0)) };
    unsafe { sail::write_field_unchecked(eng_hdl.clone(), 0, (&mut engine as *mut _) as u64) };
    sail::env_scope_ins_by_id(sl_reg, sl_env.clone(), sail::S_ENGINE.0, eng_hdl);

    crate::sail_fn! {
        let rndr_fns;
        _reg _tbl _env;

        "redraw" 1 [eng_ptr] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            engine.draw_frame();

            eng_ptr
        }

        "frame-size" 3 [eng_ptr, w, h] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            // let w = sail::u32_get(w);
            // let h = sail::u32_get(h);

            engine.need_surface_cfg = true;

            eng_ptr
        }

        "add-line" 3 [eng_ptr, points, colors] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            assert_eq!(points.core_type(), Some(sail::CoreType::VecArr));
            assert_eq!(sail::core_read_field::<u32>(points.clone(), 0), sail::T_F32.0);

            assert_eq!(colors.core_type(), Some(sail::CoreType::VecArr));
            assert_eq!(sail::core_read_field::<u32>(colors.clone(), 0), sail::T_F32.0);

            let (ln, cl) = unsafe {
                (
                    std::ptr::read_unaligned::<[f32; 4]>(points.value_ptr().add(8) as *mut _),
                    std::ptr::read_unaligned::<[f32; 3]>(colors.value_ptr().add(8) as *mut _)
                )
            };

            engine.add_line(ln, cl);

            eng_ptr
        }

        "pop-line" 1 [eng_ptr] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            engine.lines.pop();
            engine.colors.pop();

            eng_ptr
        }

        "bg-col" 4 [eng_ptr, r, g, b] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            engine.set_clear([
                sail::f32_get(r),
                sail::f32_get(g),
                sail::f32_get(b),
                1.0,
            ]);

            eng_ptr
        }

        "clear" 1 [eng_ptr] {
            assert_eq!(eng_ptr.cfg_spec(), sail::Cfg::B8Other);
            let engine = unsafe {
                &mut *(sail::read_field_unchecked::<u64>(eng_ptr.clone(), 0) as *mut Engine)
            };

            engine.empty_lines();

            eng_ptr
        }
    }

    sail::insert_native_procs(sl_reg, sl_tbl.clone(), sl_env.clone(), rndr_fns);

    engine.setup();
    engine.set_clear([1.0, 1.0, 1.0, 1.0]);

    let prog_txt = &std::fs::read_to_string("scripts/rndr.sl").unwrap();
    let prog_expr = sail::parser::parse(sl_reg, sl_tbl.clone(), prog_txt).unwrap();

    let mut stack = sail::eval::EvalStack::new(10000);

    let mut ret_slot: usize = 0;
    let ret_addr: *mut *mut sail::SlHead = (&mut ret_slot as *mut usize) as _;

    stack.start(ret_addr, sl_env.clone(), prog_expr);

    while stack.iter_once(sl_reg, sl_tbl.clone()) {}

    let rndr = sail::env_lookup_by_id(sl_env.clone(), sail::S_RNDR.0).expect("rndr script not in env");

    stack.push_frame_head(ret_addr, sail::eval::Opcode::Apply, sl_env);
    stack.push(rndr);

    loop {
        if engine.need_surface_cfg {
            engine.surface_cfg();
            engine.draw_frame();
            engine.need_surface_cfg = false;
        }

        if !stack.iter_once(sl_reg, sl_tbl.clone()) {
            println!("render thread ended");
            break;
        }
    }

    // TODO: dispose of Sail stack, region, etc
    drop(engine);
}

/// Sail-specific graphics engine state
pub struct Engine {
    clear: [f32; 4],
    lines: Vec<[f32; 4]>,
    colors: Vec<[f32; 3]>,
    buflen: u64,

    need_surface_cfg: bool,

    instance: ash::Instance,
    device: ash::Device,
    physical_device: vk::PhysicalDevice,
    dev_mem_prop: vk::PhysicalDeviceMemoryProperties,

    debug_utils_loader: ash::extensions::ext::DebugUtils,
    debug_callback: vk::DebugUtilsMessengerEXT,

    swap_loader: ash::extensions::khr::Swapchain,
    swapchain: vk::SwapchainKHR,

    surf_loader: ash::extensions::khr::Surface,
    surface: vk::SurfaceKHR,
    surface_res: vk::Extent2D,
    surface_fmt: vk::SurfaceFormatKHR,

    renderpass: vk::RenderPass,
    framebuffer: vk::Framebuffer,
    present_queue: vk::Queue,
    present_images: Vec<vk::Image>,

    viewport: vk::Viewport,
    scissor: vk::Rect2D,

    cmd_pool: vk::CommandPool,
    cmd_buffers: Vec<vk::CommandBuffer>,
    fences: Vec<vk::Fence>,

    render_semaphore: vk::Semaphore,
    present_semaphore: vk::Semaphore,

    vtx_memory: Vec<vk::DeviceMemory>,
    vtx_buffers: Vec<vk::Buffer>,

    pipeline_layouts: Vec<vk::PipelineLayout>,
    pipelines: Vec<vk::Pipeline>,
}

impl Engine {
    /// Initialize the graphics engine
    fn new(frame: FrameHandles, name: &str, width: u32, height: u32) -> Self {
        // let entry = unsafe { ash::Entry::load() }.expect("Ash entry error");
        let entry = ash::Entry::linked();
        let app_name = std::ffi::CString::new(name).unwrap();
        let app_info = vk::ApplicationInfo::builder()
            .application_name(&app_name)
            .engine_name(&app_name)
            .api_version(vk::make_api_version(0, 1, 2, 0));

        let layer_names = [std::ffi::CString::new("VK_LAYER_KHRONOS_validation").unwrap()];
        let layer_names_raw = layer_names
            .iter()
            .map(|raw| raw.as_ptr())
            .collect::<Vec<_>>();

        let surface_exts = ash_window::enumerate_required_extensions(frame.display).unwrap();
        let mut surface_ext_names_raw = surface_exts.iter().map(|raw| *raw).collect::<Vec<_>>();
        surface_ext_names_raw.push(ash::extensions::ext::DebugUtils::name().as_ptr());

        let create_info = vk::InstanceCreateInfo::builder()
            .application_info(&app_info)
            .enabled_layer_names(&layer_names_raw)
            .enabled_extension_names(&surface_ext_names_raw);

        let instance = unsafe {
            entry
                .create_instance(&create_info, None)
                .expect("Instance creation error")
        };

        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
                    | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                    | vk::DebugUtilsMessageSeverityFlagsEXT::INFO,
                // | vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE,
            )
            .message_type(
                vk::DebugUtilsMessageTypeFlagsEXT::GENERAL
                    | vk::DebugUtilsMessageTypeFlagsEXT::VALIDATION
                    | vk::DebugUtilsMessageTypeFlagsEXT::PERFORMANCE,
            )
            .pfn_user_callback(Some(Self::vk_debug_callback));

        let debug_utils_loader = ash::extensions::ext::DebugUtils::new(&entry, &instance);
        let debug_callback = unsafe {
            debug_utils_loader
                .create_debug_utils_messenger(&debug_info, None)
                .unwrap()
        };

        let surface_loader = ash::extensions::khr::Surface::new(&entry, &instance);
        let surface = unsafe {
            ash_window::create_surface(&entry, &instance, frame.display, frame.window, None)
                .unwrap()
        };

        let pdevices = unsafe {
            instance
                .enumerate_physical_devices()
                .expect("Physical device error")
        };

        let (pdevice, queue_family_index) = unsafe {
            pdevices
                .iter()
                .find_map(|pdevice| {
                    instance
                        .get_physical_device_queue_family_properties(*pdevice)
                        .iter()
                        .enumerate()
                        .find_map(|(index, info)| {
                            if info.queue_flags.contains(vk::QueueFlags::GRAPHICS)
                                && surface_loader
                                    .get_physical_device_surface_support(
                                        *pdevice,
                                        index as u32,
                                        surface,
                                    )
                                    .unwrap()
                            {
                                Some((*pdevice, index as u32))
                            } else {
                                None
                            }
                        })
                })
                .expect("Could not find suitable device.")
        };

        let device_memory_properties =
            unsafe { instance.get_physical_device_memory_properties(pdevice) };

        let priorities = [1.0];
        let queue_info = [vk::DeviceQueueCreateInfo::builder()
            .queue_family_index(queue_family_index)
            .queue_priorities(&priorities)
            .build()];

        let device_ext_names_raw = [ash::extensions::khr::Swapchain::name().as_ptr()];
        let features = vk::PhysicalDeviceFeatures::builder();

        let mut supported_features12 = vk::PhysicalDeviceVulkan12Features::default();
        let mut supported_features =
            vk::PhysicalDeviceFeatures2::builder().push_next(&mut supported_features12);

        unsafe { instance.get_physical_device_features2(pdevice, &mut supported_features) };

        if supported_features12.imageless_framebuffer == vk::FALSE {
            panic!("imageless framebuffer not supported")
        }

        let mut desired_features12 =
            vk::PhysicalDeviceVulkan12Features::builder().imageless_framebuffer(true);

        let device_create_info = vk::DeviceCreateInfo::builder()
            .queue_create_infos(&queue_info)
            .enabled_extension_names(&device_ext_names_raw)
            .enabled_features(&features)
            .push_next(&mut desired_features12);

        let device = unsafe {
            instance
                .create_device(pdevice, &device_create_info, None)
                .unwrap()
        };

        let present_queue = unsafe { device.get_device_queue(queue_family_index, 0) };

        let supported_formats = unsafe {
            surface_loader
                .get_physical_device_surface_formats(pdevice, surface)
                .unwrap()
        };

        let default_format = supported_formats[0];
        let surface_format = supported_formats
            .into_iter()
            .find(|f| f.format == vk::Format::B8G8R8A8_SRGB)
            .unwrap_or(default_format);

        let attachment = vk::AttachmentDescription::builder()
            .format(surface_format.format)
            .samples(vk::SampleCountFlags::TYPE_1)
            .load_op(vk::AttachmentLoadOp::CLEAR)
            .store_op(vk::AttachmentStoreOp::STORE)
            .final_layout(vk::ImageLayout::PRESENT_SRC_KHR);

        let attachment_ref = vk::AttachmentReference::builder()
            .attachment(0)
            .layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL);

        let subpass_attachments = [attachment_ref.build()];
        let subpass = vk::SubpassDescription::builder()
            .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
            .color_attachments(&subpass_attachments);

        let renderpass_attachments = [attachment.build()];
        let renderpass_subpasses = [subpass.build()];
        let renderpass_create_info = vk::RenderPassCreateInfo::builder()
            .attachments(&renderpass_attachments)
            .subpasses(&renderpass_subpasses);

        let renderpass = unsafe {
            device
                .create_render_pass(&renderpass_create_info, None)
                .unwrap()
        };

        let swapchain_loader = ash::extensions::khr::Swapchain::new(&instance, &device);

        let pool_create_info = vk::CommandPoolCreateInfo::builder()
            .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
            .queue_family_index(queue_family_index);

        let pool = unsafe { device.create_command_pool(&pool_create_info, None).unwrap() };

        let command_buffer_alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_buffer_count(2)
            .command_pool(pool)
            .level(vk::CommandBufferLevel::PRIMARY);

        let command_buffers = unsafe {
            device
                .allocate_command_buffers(&command_buffer_alloc_info)
                .unwrap()
        };
        // let setup_command_buffer = command_buffers[0];
        // let draw_command_buffer = command_buffers[1];

        let fence_create_info =
            vk::FenceCreateInfo::builder().flags(vk::FenceCreateFlags::SIGNALED);
        let setup_command_fence = unsafe { device.create_fence(&fence_create_info, None).unwrap() };
        let draw_command_fence = unsafe { device.create_fence(&fence_create_info, None).unwrap() };

        let semaphore_create_info = vk::SemaphoreCreateInfo::default();
        let rendering_complete_semaphore = unsafe {
            device
                .create_semaphore(&semaphore_create_info, None)
                .unwrap()
        };
        let present_complete_semaphore = unsafe {
            device
                .create_semaphore(&semaphore_create_info, None)
                .unwrap()
        };

        let (surface_res, swapchain, framebuffer, viewport, scissor, present_images) =
            Self::surface_gen(
                &device,
                &pdevice,
                &renderpass,
                &surface_loader,
                &surface,
                &surface_format,
                &swapchain_loader,
            );

        Self {
            clear: [0.0, 0.0, 0.0, 1.0],
            lines: vec![],
            colors: vec![],
            buflen: 256,

            need_surface_cfg: false,

            instance,
            device,
            physical_device: pdevice,
            dev_mem_prop: device_memory_properties,

            debug_utils_loader,
            debug_callback,

            renderpass,

            surf_loader: surface_loader,
            surface,
            surface_res,
            surface_fmt: surface_format,

            swap_loader: swapchain_loader,
            swapchain,

            framebuffer,
            present_images,

            viewport,
            scissor,

            cmd_pool: pool,
            cmd_buffers: command_buffers,
            fences: vec![setup_command_fence, draw_command_fence],

            present_queue,

            render_semaphore: rendering_complete_semaphore,
            present_semaphore: present_complete_semaphore,

            vtx_buffers: vec![],
            vtx_memory: vec![],

            pipeline_layouts: vec![],
            pipelines: vec![],
        }
    }

    unsafe extern "system" fn vk_debug_callback(
        message_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
        message_type: vk::DebugUtilsMessageTypeFlagsEXT,
        p_callback_data: *const vk::DebugUtilsMessengerCallbackDataEXT,
        _user_data: *mut std::os::raw::c_void,
    ) -> vk::Bool32 {
        let callback_data = *p_callback_data;
        let message_id_number: i32 = callback_data.message_id_number;

        let message_id_name = if callback_data.p_message_id_name.is_null() {
            std::borrow::Cow::from("")
        } else {
            std::ffi::CStr::from_ptr(callback_data.p_message_id_name).to_string_lossy()
        };

        let message = if callback_data.p_message.is_null() {
            std::borrow::Cow::from("")
        } else {
            std::ffi::CStr::from_ptr(callback_data.p_message).to_string_lossy()
        };

        match message_severity {
            vk::DebugUtilsMessageSeverityFlagsEXT::ERROR => log::error!(
                "{:?} [{} ({})] : {}",
                message_type,
                message_id_name,
                &message_id_number.to_string(),
                message
            ),
            vk::DebugUtilsMessageSeverityFlagsEXT::WARNING => log::warn!(
                "{:?} [{} ({})] : {}",
                message_type,
                message_id_name,
                &message_id_number.to_string(),
                message
            ),
            vk::DebugUtilsMessageSeverityFlagsEXT::INFO => log::info!(
                "{:?} [{} ({})] : {}",
                message_type,
                message_id_name,
                &message_id_number.to_string(),
                message
            ),
            vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE => log::trace!(
                "{:?} [{} ({})] : {}",
                message_type,
                message_id_name,
                &message_id_number.to_string(),
                message
            ),
            _ => (),
        }

        vk::FALSE
    }

    /// Generate Vulkan objects that change when the surface changes
    fn surface_gen(
        device: &ash::Device,
        pdevice: &vk::PhysicalDevice,
        renderpass: &vk::RenderPass,
        surface_loader: &ash::extensions::khr::Surface,
        surface: &vk::SurfaceKHR,
        surface_format: &vk::SurfaceFormatKHR,
        swapchain_loader: &ash::extensions::khr::Swapchain,
    ) -> (
        vk::Extent2D,
        vk::SwapchainKHR,
        vk::Framebuffer,
        vk::Viewport,
        vk::Rect2D,
        Vec<vk::Image>,
    ) {
        let surface_capabilities = unsafe {
            surface_loader
                .get_physical_device_surface_capabilities(*pdevice, *surface)
                .unwrap()
        };

        let desired_image_count =
            if surface_capabilities.min_image_count == surface_capabilities.max_image_count {
                surface_capabilities.max_image_count
            } else {
                surface_capabilities.min_image_count + 1
            };

        let surface_resolution = match surface_capabilities.current_extent.width {
            u32::MAX => vk::Extent2D::default(),
            _ => surface_capabilities.current_extent,
        };

        let surface_transform = if surface_capabilities
            .supported_transforms
            .contains(vk::SurfaceTransformFlagsKHR::IDENTITY)
        {
            vk::SurfaceTransformFlagsKHR::IDENTITY
        } else {
            surface_capabilities.current_transform
        };

        let attachment_image_info = vk::FramebufferAttachmentImageInfo::builder()
            .usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .width(surface_resolution.width)
            .height(surface_resolution.height)
            .layer_count(1)
            .view_formats(std::slice::from_ref(&surface_format.format));

        let attachment_image_infos = [attachment_image_info.build()];

        let mut attachments_create_info = vk::FramebufferAttachmentsCreateInfo::builder()
            .attachment_image_infos(&attachment_image_infos);

        let false_image = vk::ImageView::default();
        let framebuffer_create_info = vk::FramebufferCreateInfo::builder()
            .flags(vk::FramebufferCreateFlags::IMAGELESS)
            .render_pass(*renderpass)
            .attachments(std::slice::from_ref(&false_image))
            .width(surface_resolution.width)
            .height(surface_resolution.height)
            .layers(1)
            .push_next(&mut attachments_create_info);

        let framebuffer = unsafe {
            device
                .create_framebuffer(&framebuffer_create_info, None)
                .unwrap()
        };

        let present_modes = unsafe {
            surface_loader
                .get_physical_device_surface_present_modes(*pdevice, *surface)
                .unwrap()
        };

        let present_mode = present_modes
            .iter()
            .find(|&&mode| mode == vk::PresentModeKHR::MAILBOX)
            .map(|m| *m)
            .unwrap_or(vk::PresentModeKHR::FIFO);

        let swapchain_create_info = vk::SwapchainCreateInfoKHR::builder()
            .surface(*surface)
            .min_image_count(desired_image_count)
            .image_color_space(surface_format.color_space)
            .image_format(surface_format.format)
            .image_extent(surface_resolution)
            .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .image_sharing_mode(vk::SharingMode::EXCLUSIVE)
            .pre_transform(surface_transform)
            .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
            .present_mode(present_mode)
            .clipped(true)
            .image_array_layers(1);

        let swapchain = unsafe {
            swapchain_loader
                .create_swapchain(&swapchain_create_info, None)
                .unwrap()
        };

        let present_images = unsafe { swapchain_loader.get_swapchain_images(swapchain).unwrap() };

        let viewport = vk::Viewport::builder()
            .x(0.0)
            .y(0.0)
            .width(surface_resolution.width as f32)
            .height(surface_resolution.height as f32)
            .max_depth(1.0);

        let scissor = vk::Rect2D::builder().extent(surface_resolution);

        (
            surface_resolution,
            swapchain,
            framebuffer,
            viewport.build(),
            scissor.build(),
            present_images,
        )
    }

    /// Reconfigure objects linked to surface state
    fn surface_cfg(&mut self) {
        unsafe {
            self.device
                .wait_for_fences(&self.fences, true, 1_000_000_000)
                .unwrap();
        }

        let (old_swapchain, old_framebuffer) = (self.swapchain, self.framebuffer);

        unsafe {
            self.device.destroy_framebuffer(old_framebuffer, None);
            self.swap_loader.destroy_swapchain(old_swapchain, None);
        }

        (
            self.surface_res,
            self.swapchain,
            self.framebuffer,
            self.viewport,
            self.scissor,
            self.present_images,
        ) = Self::surface_gen(
            &self.device,
            &self.physical_device,
            &self.renderpass,
            &self.surf_loader,
            &self.surface,
            &self.surface_fmt,
            &self.swap_loader,
        );
    }

    /// Acquire memory and create buffer for vertex data
    fn state_buffer_setup(&mut self) {
        unsafe {
            self.device
                .wait_for_fences(&[self.fences[1]], true, 1_000_000_000)
                .unwrap();

            for mem in self.vtx_memory.drain(..) {
                self.device.free_memory(mem, None);
            }

            for buf in self.vtx_buffers.drain(..) {
                self.device.destroy_buffer(buf, None);
            }
        }

        let (memory, buffer) = unsafe {
            self.mk_buffer(
                self.buflen,
                vk::BufferUsageFlags::VERTEX_BUFFER,
                vk::MemoryPropertyFlags::HOST_VISIBLE,
            )
        };

        self.vtx_memory.push(memory);
        self.vtx_buffers.push(buffer);
    }

    /// Set up an appropriate graphics pipeline for the engine
    fn state_pipeline_setup(&mut self) {
        let push_constant_range = vk::PushConstantRange::builder()
            .stage_flags(vk::ShaderStageFlags::FRAGMENT)
            .offset(0)
            .size(12);

        let push_constant_ranges = [push_constant_range.build()];

        let layout_create_info =
            vk::PipelineLayoutCreateInfo::builder().push_constant_ranges(&push_constant_ranges);

        let pipeline_layout = unsafe {
            self.device
                .create_pipeline_layout(&layout_create_info, None)
                .unwrap()
        };

        let vertex_shader = include_str!("shaders/lines.vert");
        let fragment_shader = include_str!("shaders/lines.frag");

        let pipeline = unsafe {
            self.mk_pipeline(
                pipeline_layout,
                vertex_shader,
                fragment_shader,
                vk::PrimitiveTopology::LINE_LIST,
            )
        };

        self.pipeline_layouts.push(pipeline_layout);
        self.pipelines.push(pipeline);
    }

    /// Create a new buffer for graphics processing and bind its memory
    unsafe fn mk_buffer(
        &self,
        buffer_len: u64,
        usage: vk::BufferUsageFlags,
        properties: vk::MemoryPropertyFlags,
    ) -> (vk::DeviceMemory, vk::Buffer) {
        let buffer = {
            let buf_create_info = vk::BufferCreateInfo::builder()
                .size(buffer_len)
                .usage(usage);
            self.device.create_buffer(&buf_create_info, None).unwrap()
        };

        let req = self.device.get_buffer_memory_requirements(buffer);

        let mem_types = self.dev_mem_prop.memory_types
            [0..self.dev_mem_prop.memory_type_count as usize]
            .to_vec();

        let buf_mem = {
            let mem_type_idx = mem_types
                .into_iter()
                .enumerate()
                .find(|(id, ty)| {
                    (req.memory_type_bits & (1_u32 << id) != 0)
                        && ty.property_flags.contains(properties)
                })
                .map(|(id, _ty)| id as u32)
                .unwrap();

            let mem_alloc_info = vk::MemoryAllocateInfo::builder()
                .allocation_size(req.size)
                .memory_type_index(mem_type_idx);

            self.device.allocate_memory(&mem_alloc_info, None).unwrap()
        };

        self.device.bind_buffer_memory(buffer, buf_mem, 0).unwrap();

        (buf_mem, buffer)
    }

    /// Generate a basic graphics pipeline
    unsafe fn mk_pipeline(
        &self,
        pipeline_layout: vk::PipelineLayout,
        vertex_shader: &str,
        fragment_shader: &str,
        primitive_type: vk::PrimitiveTopology,
    ) -> vk::Pipeline {
        let vtx_shader_mod = {
            let shader_code = Self::compile_shader(vertex_shader, shaderc::ShaderKind::Vertex);
            let shader_mod_create_info = vk::ShaderModuleCreateInfo::builder().code(&shader_code);
            self.device
                .create_shader_module(&shader_mod_create_info, None)
                .unwrap()
        };

        let frag_shader_mod = {
            let shader_code = Self::compile_shader(fragment_shader, shaderc::ShaderKind::Fragment);
            let shader_mod_create_info = vk::ShaderModuleCreateInfo::builder().code(&shader_code);
            self.device
                .create_shader_module(&shader_mod_create_info, None)
                .unwrap()
        };

        let entry_name = std::ffi::CStr::from_bytes_with_nul_unchecked(b"main\0");

        let shader_stage_info = [
            vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::VERTEX)
                .module(vtx_shader_mod)
                .name(entry_name)
                .build(),
            vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::FRAGMENT)
                .module(frag_shader_mod)
                .name(entry_name)
                .build(),
        ];

        let line_binding = vk::VertexInputBindingDescription::builder()
            .binding(0)
            .stride((size_of::<f32>() * 2) as u32)
            .input_rate(vk::VertexInputRate::VERTEX);

        let line_attribute = vk::VertexInputAttributeDescription::builder()
            .location(0)
            .binding(0)
            .format(vk::Format::R32G32_SFLOAT)
            .offset(0);

        let vertex_bindings = [line_binding.build()];

        let vertex_attributes = [line_attribute.build()];

        let vertex_input_state_info = vk::PipelineVertexInputStateCreateInfo::builder()
            .vertex_binding_descriptions(&vertex_bindings)
            .vertex_attribute_descriptions(&vertex_attributes);

        let input_assembly_state_info =
            vk::PipelineInputAssemblyStateCreateInfo::builder().topology(primitive_type);

        let viewport_state_info = vk::PipelineViewportStateCreateInfo::builder()
            .viewport_count(1)
            .scissor_count(1);

        let rasterization_state_info = vk::PipelineRasterizationStateCreateInfo::builder()
            .polygon_mode(vk::PolygonMode::FILL)
            .line_width(1.0);

        let multisample_state_info = vk::PipelineMultisampleStateCreateInfo::builder()
            .rasterization_samples(vk::SampleCountFlags::TYPE_1);

        let attachment_blend = vk::PipelineColorBlendAttachmentState::builder()
            .blend_enable(false)
            .color_write_mask(
                vk::ColorComponentFlags::R
                    | vk::ColorComponentFlags::G
                    | vk::ColorComponentFlags::B
                    | vk::ColorComponentFlags::A,
            );

        let blend_attachments = [attachment_blend.build()];

        let color_blend_state_info =
            vk::PipelineColorBlendStateCreateInfo::builder().attachments(&blend_attachments);

        let dynamic_state_info = vk::PipelineDynamicStateCreateInfo::builder()
            .dynamic_states(&[vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR]);

        let pipeline_create_info = vk::GraphicsPipelineCreateInfo::builder()
            .stages(&shader_stage_info)
            .vertex_input_state(&vertex_input_state_info)
            .input_assembly_state(&input_assembly_state_info)
            .viewport_state(&viewport_state_info)
            .rasterization_state(&rasterization_state_info)
            .multisample_state(&multisample_state_info)
            .color_blend_state(&color_blend_state_info)
            .dynamic_state(&dynamic_state_info)
            .layout(pipeline_layout)
            .render_pass(self.renderpass)
            .subpass(0);

        let pipeline_create_infos = [pipeline_create_info.build()];

        let pipeline = self
            .device
            .create_graphics_pipelines(vk::PipelineCache::null(), &pipeline_create_infos, None)
            .unwrap()[0];

        self.device.destroy_shader_module(vtx_shader_mod, None);
        self.device.destroy_shader_module(frag_shader_mod, None);

        pipeline
    }

    /// Compile GLSL shader code into SPIR-V
    fn compile_shader(glsl: &str, shader_kind: shaderc::ShaderKind) -> Vec<u32> {
        let compiler = shaderc::Compiler::new().unwrap();

        let compiled_shader = compiler
            .compile_into_spirv(glsl, shader_kind, "unnamed", "main", None)
            .expect("Failed to compile shader");

        compiled_shader.as_binary().to_vec()
    }

    /// Set the clear (background) color
    fn set_clear(&mut self, clear: [f32; 4]) {
        self.clear = clear;
    }

    /// Add a line, with two endpoints and a color
    fn add_line(&mut self, points: [f32; 4], color: [f32; 3]) {
        self.lines.push(points);
        self.colors.push(color);
        self.buffer_size_check();
    }

    /// Empty the engine of all lines
    fn empty_lines(&mut self) {
        self.lines.clear();
        self.colors.clear();
    }

    /// Check whether the buffer has enough space for all vertices
    fn buffer_size_check(&mut self) {
        let line_vec_size = size_of::<[f32; 4]>() * self.lines.len();

        if line_vec_size as u64 >= self.buflen {
            self.buflen *= 2;
            self.state_buffer_setup();
        }
    }

    /// Prepare the engine to accept draw calls
    fn setup(&mut self) {
        self.state_buffer_setup();
        self.state_pipeline_setup();
    }

    /// Draw a single frame according to the engine state
    fn draw_frame(&mut self) {
        let timeout_ns = 1_000_000_000;

        unsafe {
            self.device
                .wait_for_fences(&[self.fences[1]], true, timeout_ns)
                .unwrap();
            self.device.reset_fences(&[self.fences[1]]).unwrap();
            self.device
                .reset_command_buffer(self.cmd_buffers[1], vk::CommandBufferResetFlags::empty())
                .unwrap();
        }

        let (present_index, _) = unsafe {
            match self.swap_loader.acquire_next_image(
                self.swapchain,
                timeout_ns,
                self.present_semaphore,
                vk::Fence::null(),
            ) {
                Ok(result) => result,
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.need_surface_cfg = true;
                    return;
                }
                Err(_) => return,
            }
        };

        let line_vec_size = size_of::<[f32; 4]>() * self.lines.len();

        if line_vec_size > 0 {
            unsafe {
                let mapped_mem = self
                    .device
                    .map_memory(
                        self.vtx_memory[0],
                        0,
                        vk::WHOLE_SIZE,
                        vk::MemoryMapFlags::empty(),
                    )
                    .unwrap() as *mut u8;

                std::ptr::copy_nonoverlapping(
                    self.lines.as_ptr() as *const u8,
                    mapped_mem,
                    line_vec_size,
                );

                self.device
                    .flush_mapped_memory_ranges(&[vk::MappedMemoryRange::builder()
                        .memory(self.vtx_memory[0])
                        .offset(0)
                        .size(vk::WHOLE_SIZE)
                        .build()])
                    .unwrap();

                self.device.unmap_memory(self.vtx_memory[0]);
            }
        }

        let image_view = unsafe {
            let image_view_create_info = vk::ImageViewCreateInfo::builder()
                .image(self.present_images[present_index as usize])
                .view_type(vk::ImageViewType::TYPE_2D)
                .format(self.surface_fmt.format)
                .components(vk::ComponentMapping {
                    r: vk::ComponentSwizzle::IDENTITY,
                    g: vk::ComponentSwizzle::IDENTITY,
                    b: vk::ComponentSwizzle::IDENTITY,
                    a: vk::ComponentSwizzle::IDENTITY,
                })
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                });

            self.device
                .create_image_view(&image_view_create_info, None)
                .unwrap()
        };

        unsafe {
            let clear_value = vk::ClearValue {
                color: vk::ClearColorValue {
                    float32: self.clear,
                },
            };

            let clear_values = [clear_value];

            let mut render_pass_attachment_begin_info =
                vk::RenderPassAttachmentBeginInfo::builder()
                    .attachments(std::slice::from_ref(&image_view));

            let render_pass_begin_info = vk::RenderPassBeginInfo::builder()
                .render_pass(self.renderpass)
                .framebuffer(self.framebuffer)
                .render_area(vk::Rect2D::builder().extent(self.surface_res).build())
                .clear_values(&clear_values)
                .push_next(&mut render_pass_attachment_begin_info);

            let cmd_buffer_begin_info = vk::CommandBufferBeginInfo::builder()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);

            self.device
                .begin_command_buffer(self.cmd_buffers[1], &cmd_buffer_begin_info)
                .unwrap();

            self.device.cmd_begin_render_pass(
                self.cmd_buffers[1],
                &render_pass_begin_info,
                vk::SubpassContents::INLINE,
            );

            self.device.cmd_bind_pipeline(
                self.cmd_buffers[1],
                vk::PipelineBindPoint::GRAPHICS,
                self.pipelines[0],
            );

            self.device
                .cmd_set_viewport(self.cmd_buffers[1], 0, &[self.viewport]);
            self.device
                .cmd_set_scissor(self.cmd_buffers[1], 0, &[self.scissor]);

            self.device.cmd_bind_vertex_buffers(
                self.cmd_buffers[1],
                0,
                &[self.vtx_buffers[0]],
                &[0],
            );

            for l in 0..self.lines.len() {
                self.device.cmd_push_constants(
                    self.cmd_buffers[1],
                    self.pipeline_layouts[0],
                    vk::ShaderStageFlags::FRAGMENT,
                    0,
                    std::slice::from_raw_parts((&self.colors[l]).as_ptr() as *const u8, 12),
                );
                let ind = (2 * l) as u32;
                self.device.cmd_draw(self.cmd_buffers[1], 2, 1, ind, 0);
            }

            self.device.cmd_end_render_pass(self.cmd_buffers[1]);

            self.device.end_command_buffer(self.cmd_buffers[1]).unwrap();
        }

        unsafe {
            let submit_info = vk::SubmitInfo::builder()
                .wait_semaphores(std::slice::from_ref(&self.present_semaphore))
                .wait_dst_stage_mask(std::slice::from_ref(&vk::PipelineStageFlags::ALL_GRAPHICS))
                .command_buffers(std::slice::from_ref(&self.cmd_buffers[1]))
                .signal_semaphores(std::slice::from_ref(&self.render_semaphore));

            self.device
                .queue_submit(self.present_queue, &[submit_info.build()], self.fences[1])
                .unwrap();

            let present_info = vk::PresentInfoKHR::builder()
                .wait_semaphores(std::slice::from_ref(&self.render_semaphore))
                .swapchains(std::slice::from_ref(&self.swapchain))
                .image_indices(std::slice::from_ref(&present_index));

            // TODO: handle possible errors here (and everywhere)
            match self
                .swap_loader
                .queue_present(self.present_queue, &present_info)
            {
                Ok(_) => (),
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.need_surface_cfg = true;
                }
                Err(err) => panic!("{:?}", err),
            }

            self.device.destroy_image_view(image_view, None);
        }
    }
}

impl Drop for Engine {
    fn drop(&mut self) {
        unsafe {
            self.device.device_wait_idle().unwrap();
            self.device.destroy_semaphore(self.present_semaphore, None);
            self.device.destroy_semaphore(self.render_semaphore, None);
            for fence in self.fences.drain(..) {
                self.device.destroy_fence(fence, None);
            }
            for mem in self.vtx_memory.drain(..) {
                self.device.free_memory(mem, None);
            }
            for buf in self.vtx_buffers.drain(..) {
                self.device.destroy_buffer(buf, None);
            }
            for pipeline in self.pipelines.drain(..) {
                self.device.destroy_pipeline(pipeline, None);
            }
            for pipeline_layout in self.pipeline_layouts.drain(..) {
                self.device.destroy_pipeline_layout(pipeline_layout, None);
            }
            self.device.destroy_render_pass(self.renderpass, None);
            self.device.destroy_framebuffer(self.framebuffer, None);
            self.device.destroy_command_pool(self.cmd_pool, None);
            self.swap_loader.destroy_swapchain(self.swapchain, None);
            self.device.destroy_device(None);
            self.surf_loader.destroy_surface(self.surface, None);
            self.debug_utils_loader
                .destroy_debug_utils_messenger(self.debug_callback, None);
            self.instance.destroy_instance(None);
        }
    }
}

// pub fn draw_clear_frame(&mut self, color: [f32; 4]) -> Result<(), &str> {
//     let timeout_ns = 1_000_000_000;

//     unsafe {
//         self.device
//             .wait_for_fence(self.submission_complete_fence.as_ref().unwrap(), timeout_ns)
//             .unwrap();
//         self.device
//             .reset_fence(self.submission_complete_fence.as_mut().unwrap())
//             .unwrap();

//         self.command_pool.as_mut().unwrap().reset(false);
//     }

//     let surface_image = unsafe {
//         match self.surface.as_mut().unwrap().acquire_image(timeout_ns) {
//             Ok((image, _)) => image,
//             Err(_) => return Err("Could not acquire image"),
//         }
//     };

//     unsafe {
//         let buffer = &mut self.command_buffers[0];

//         buffer.begin_primary(CommandBufferFlags::ONE_TIME_SUBMIT);

//         buffer.begin_render_pass(
//             &self.render_passes[0],
//             &self.framebuffer.as_ref().unwrap(),
//             Rect {
//                 x: 0,
//                 y: 0,
//                 w: self.surface_extent.width as i16,
//                 h: self.surface_extent.height as i16,
//             },
//             iter::once(RenderAttachmentInfo {
//                 image_view: surface_image.borrow(),
//                 clear_value: ClearValue {
//                     color: ClearColor { float32: color },
//                 },
//             }),
//             SubpassContents::Inline,
//         );

//         buffer.end_render_pass();
//         buffer.finish();
//     }

//     unsafe {
//         self.queue_group.queues[0].submit(
//             vec![&self.command_buffers[0]].into_iter(),
//             vec![].into_iter(),
//             vec![self.rendering_complete_semaphore.as_ref().unwrap()].into_iter(),
//             self.submission_complete_fence.as_mut(),
//         );

//         self.queue_group.queues[0]
//             .present(
//                 self.surface.as_mut().unwrap(),
//                 surface_image,
//                 self.rendering_complete_semaphore.as_mut(),
//             )
//             .unwrap();
//     }

//     Ok(())
// }
