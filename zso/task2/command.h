// DoomDev interruptible command wrappers. All of them assume that
// dev->dev_mutex is already taken (and they don't change that state).

#ifndef COMM_H
#define COMM_H

#include "common.h"
#include <linux/dmapool.h>

// Helper functions.
void doomcmd_interrupts_on(struct harddoom_dev *dev, u32 intr_mask);
void doomcmd_interrupts_off(struct harddoom_dev *dev, u32 intr_mask);
int doomcmd_interrupts_await(struct harddoom_dev *dev, u32 intr_mask);
void doomcmd_clear_interrupts(struct harddoom_dev *dev, u32 intr_mask);
int doomcmd_synchronize(struct harddoom_dev *dev);

// DoomDev functions.
int doomcmd_interlock(struct harddoom_dev *dev);

int doomcmd_surf_dst_pt(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_surf_src_pt(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_texture_pt(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_flat_addr(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_colormap_addr(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_translation_addr(struct harddoom_dev *dev, dma_addr_t ptr);
int doomcmd_texture_dims(struct harddoom_dev *dev, u16 height, u16 sz);
int doomcmd_surf_dims(struct harddoom_dev *dev, u16 width, u16 height);

int doomcmd_fill_color(struct harddoom_dev *dev, u8 color);
int doomcmd_draw_params(struct harddoom_dev *dev, u8 flags);
int doomcmd_xy_a(struct harddoom_dev *dev, u16 x, u16 y);
int doomcmd_xy_b(struct harddoom_dev *dev, u16 x, u16 y);
int doomcmd_ustart(struct harddoom_dev *dev, u32 coord);
int doomcmd_vstart(struct harddoom_dev *dev, u32 coord);
int doomcmd_ustep(struct harddoom_dev *dev, u32 delta);
int doomcmd_vstep(struct harddoom_dev *dev, u32 delta);

int doomcmd_copy_rect(struct harddoom_dev *dev, u16 width, u16 height);
int doomcmd_fill_rect(struct harddoom_dev *dev, u16 width, u16 height);
int doomcmd_draw_line(struct harddoom_dev *dev);
int doomcmd_draw_background(struct harddoom_dev *dev);
int doomcmd_draw_column(struct harddoom_dev *dev, u32 offset);
int doomcmd_draw_span(struct harddoom_dev *dev);

#endif