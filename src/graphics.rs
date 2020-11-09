use crate::context::Context;

use gfx_hal::{
    adapter::{Adapter, PhysicalDevice},
    command::{ClearColor, ClearValue, CommandBuffer, CommandBufferFlags, SubpassContents},
    device::Device,
    format::{ChannelType, Format},
    image::{Extent, Layout},
    pass::{Attachment, AttachmentLoadOp, AttachmentOps, AttachmentStoreOp, SubpassDesc},
    pool::CommandPool,
    pso::Rect,
    queue::{CommandQueue, QueueFamily, QueueGroup, Submission},
    window::{Extent2D, PresentationSurface, Surface, SwapchainConfig},
    Instance,
};

use std::borrow::Borrow;

/// Stores all `gfx-hal` objects which must persist
/// TODO: the device and instance fields do not need to be Option
pub struct GraphicsState<B: gfx_hal::Backend> {
    surface_extent: Extent2D,
    instance: Option<B::Instance>,
    surface: Option<B::Surface>,
    adapter: Adapter<B>,
    device: Option<B::Device>,
    queue_group: QueueGroup<B>,
    surface_color_format: Format,
    render_passes: Vec<B::RenderPass>,
    command_pool: Option<B::CommandPool>,
    command_buffers: Vec<B::CommandBuffer>,
    submission_complete_fence: Option<B::Fence>,
    rendering_complete_semaphore: Option<B::Semaphore>,
}

/// Initialize the graphics system and track necessary state
impl<B: gfx_hal::Backend> GraphicsState<B> {
    pub fn new(context: &Context, name: &str, width: u32, height: u32) -> Self {
        let surface_extent = Extent2D { width, height };
        let instance = B::Instance::create(name, 1).unwrap();
        let surface = unsafe { instance.create_surface(&context.window).unwrap() };
        let adapter = instance
            .enumerate_adapters()
            .into_iter()
            .find(|a| {
                a.queue_families.iter().any(|qf| {
                    surface.supports_queue_family(qf) && qf.queue_type().supports_graphics()
                })
            })
            .unwrap();
        let queue_family = adapter
            .queue_families
            .iter()
            .find(|qf| surface.supports_queue_family(qf) && qf.queue_type().supports_graphics())
            .unwrap();
        let mut gpu = unsafe {
            adapter
                .physical_device
                .open(&[(queue_family, &[1.0])], gfx_hal::Features::empty())
                .unwrap()
        };
        let device = gpu.device;
        let queue_group = gpu.queue_groups.remove(queue_family.id().0);
        let supported_formats = surface
            .supported_formats(&adapter.physical_device)
            .unwrap_or(vec![]);
        let default_format = *supported_formats.get(0).unwrap_or(&Format::Rgba8Srgb);
        let surface_color_format = supported_formats
            .into_iter()
            .find(|f| f.base_format().1 == ChannelType::Srgb)
            .unwrap_or(default_format);
        let color_attachment = Attachment {
            format: Some(surface_color_format),
            samples: 1,
            ops: AttachmentOps::new(AttachmentLoadOp::Clear, AttachmentStoreOp::Store),
            stencil_ops: AttachmentOps::DONT_CARE,
            layouts: Layout::Undefined..Layout::Present,
        };
        let subpass = SubpassDesc {
            colors: &[(0, Layout::ColorAttachmentOptimal)],
            depth_stencil: None,
            inputs: &[],
            resolves: &[],
            preserves: &[],
        };
        let render_pass = unsafe {
            device
                .create_render_pass(&[color_attachment], &[subpass], &[])
                .unwrap()
        };
        let mut command_pool = unsafe {
            device
                .create_command_pool(
                    queue_group.family,
                    gfx_hal::pool::CommandPoolCreateFlags::empty(),
                )
                .unwrap()
        };
        let command_buffer = unsafe { command_pool.allocate_one(gfx_hal::command::Level::Primary) };
        let submission_complete_fence = device.create_fence(false).unwrap();
        let rendering_complete_semaphore = device.create_semaphore().unwrap();

        Self {
            surface_extent,
            instance: Some(instance),
            surface: Some(surface),
            adapter,
            device: Some(device),
            queue_group,
            surface_color_format,
            render_passes: vec![render_pass],
            command_pool: Some(command_pool),
            command_buffers: vec![command_buffer],
            submission_complete_fence: Some(submission_complete_fence),
            rendering_complete_semaphore: Some(rendering_complete_semaphore),
        }
    }

    /// Draw a frame that is cleared to the specified color
    pub fn draw_clear_frame(&mut self, color: [f32; 4]) -> Result<(), &str> {
        let timeout_ns = 1_000_000_000;

        unsafe {
            self.device
                .as_ref()
                .unwrap()
                .wait_for_fence(self.submission_complete_fence.as_ref().unwrap(), timeout_ns)
                .unwrap();
            self.device
                .as_ref()
                .unwrap()
                .reset_fence(self.submission_complete_fence.as_ref().unwrap())
                .unwrap();
        }

        let surface_image = unsafe {
            match self.surface.as_mut().unwrap().acquire_image(timeout_ns) {
                Ok((image, _)) => image,
                Err(_) => return Err("Could not acquire image"),
            }
        };

        let framebuffer = unsafe {
            self.device
                .as_ref()
                .unwrap()
                .create_framebuffer(
                    &self.render_passes[0],
                    vec![surface_image.borrow()],
                    Extent {
                        width: self.surface_extent.width,
                        height: self.surface_extent.height,
                        depth: 1,
                    },
                )
                .unwrap()
        };

        unsafe {
            self.command_pool.as_mut().unwrap().reset(false);
        }

        unsafe {
            let buffer = &mut self.command_buffers[0];

            // buffer.reset(false);

            buffer.begin_primary(CommandBufferFlags::ONE_TIME_SUBMIT);

            buffer.begin_render_pass(
                &self.render_passes[0],
                &framebuffer,
                Rect {
                    x: 0,
                    y: 0,
                    w: self.surface_extent.width as i16,
                    h: self.surface_extent.height as i16,
                },
                &[ClearValue {
                    color: ClearColor { float32: color },
                }],
                SubpassContents::Inline,
            );

            buffer.end_render_pass();
            buffer.finish();
        }

        unsafe {
            let submission = Submission {
                command_buffers: vec![&self.command_buffers[0]],
                wait_semaphores: None,
                signal_semaphores: vec![self.rendering_complete_semaphore.as_ref().unwrap()],
            };

            self.queue_group.queues[0].submit(submission, self.submission_complete_fence.as_ref());

            self.queue_group.queues[0]
                .present(
                    self.surface.as_mut().unwrap(),
                    surface_image,
                    self.rendering_complete_semaphore.as_ref(),
                )
                .unwrap();

            self.device
                .as_ref()
                .unwrap()
                .destroy_framebuffer(framebuffer);
        }

        Ok(())
    }

    /// Reconfigure the swapchain (usually due to new surface extent)
    pub fn config_swapchain(&mut self) {
        let caps = self
            .surface
            .as_ref()
            .unwrap()
            .capabilities(&self.adapter.physical_device);
        let swapchain_config =
            SwapchainConfig::from_caps(&caps, self.surface_color_format, self.surface_extent);
        self.surface_extent = swapchain_config.extent;

        unsafe {
            self.surface
                .as_mut()
                .unwrap()
                .configure_swapchain(self.device.as_ref().unwrap(), swapchain_config)
                .unwrap();
        }
    }

    pub fn set_extent(&mut self, width: u32, height: u32) {
        self.surface_extent = Extent2D { width, height };
    }
}

impl<B: gfx_hal::Backend> Drop for GraphicsState<B> {
    fn drop(&mut self) {
        unsafe {
            self.device
                .as_ref()
                .unwrap()
                .wait_for_fence(self.submission_complete_fence.as_ref().unwrap(), 1_000_000_000)
                .unwrap();
            self.device
                .as_ref()
                .unwrap()
                .destroy_semaphore(self.rendering_complete_semaphore.take().unwrap());
            self.device
                .as_ref()
                .unwrap()
                .destroy_fence(self.submission_complete_fence.take().unwrap());
            for render_pass in self.render_passes.drain(..) {
                self.device
                    .as_ref()
                    .unwrap()
                    .destroy_render_pass(render_pass);
            }
            self.device
                .as_ref()
                .unwrap()
                .destroy_command_pool(self.command_pool.take().unwrap());
            self.surface
                .as_mut()
                .unwrap()
                .unconfigure_swapchain(self.device.as_ref().unwrap());
            self.instance
                .as_ref()
                .unwrap()
                .destroy_surface(self.surface.take().unwrap());
        }
    }
}
