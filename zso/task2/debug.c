#include "debug.h"
#include "harddoom.h"

#define HARDDOOM_INTR_ERROR 0x000003f8
#define HARDDOOM_INTR_OVERFLOW                                                 \
	(HARDDOOM_INTR_SURF_DST_OVERFLOW | HARDDOOM_INTR_SURF_SRC_OVERFLOW)
#define HARDDOOM_INTR_DST                                                      \
	(HARDDOOM_INTR_SURF_DST_OVERFLOW | HARDDOOM_INTR_PAGE_FAULT_SURF_DST)
#define HARDDOOM_INTR_SRC                                                      \
	(HARDDOOM_INTR_SURF_SRC_OVERFLOW | HARDDOOM_INTR_PAGE_FAULT_SURF_SRC)
#define PREF "[HDOOM] BUG: "
#define PREF_STAT "[HDOOM] STATS: "

// Spinlock needs (unfortunately) to be taken here.
void harddoom_bugcheck(struct harddoom_dev *hddev)
{
	void __iomem *bar_ptr = hddev->bar_iomap;
	u32 mask = atomic_read(&hddev->interrupts);

	if (likely(!(mask & HARDDOOM_INTR_ERROR)))
		return;

	printk(KERN_ERR PREF "Interrupt mask is 0x%03x", mask);
	if (mask & HARDDOOM_INTR_FE_ERROR) {
		printk(KERN_ERR PREF "FE_ERROR is present");
		printk(KERN_ERR PREF "FE_ERROR_CODE is 0x%02x",
		       ioread32(bar_ptr + HARDDOOM_FE_ERROR_CODE));
		printk(KERN_ERR PREF "FE_ERROR_DATA is 0x%08x",
		       ioread32(bar_ptr + HARDDOOM_FE_ERROR_CMD));
	}
	if (mask & HARDDOOM_INTR_OVERFLOW) {
		u32 val = ioread32(bar_ptr + HARDDOOM_XY_SURF_DIMS);
		printk(KERN_ERR PREF "Info for SURF_DIMS: w(%u) h(%u)",
		       val & 0x3f, (val >> 8) & 0xfff);
	}
	if (mask & HARDDOOM_INTR_DST) {
		u32 val = ioread32(bar_ptr + HARDDOOM_XY_DST_CMD);
		printk(KERN_ERR PREF "Info for DST-type error: x(%u) y(%u)",
		       val & 0x1f, (val >> 5) & 0x7ff);
	}
	if (mask & HARDDOOM_INTR_SRC) {
		u32 val = ioread32(bar_ptr + HARDDOOM_XY_SRC_CMD);
		printk(KERN_ERR PREF "Info for SRC-type error: x(%u) y(%u)",
		       val & 0x1f, (val >> 5) & 0x7ff);
	}
	if (mask & HARDDOOM_INTR_PAGE_FAULT_SURF_DST) {
		printk(KERN_ERR PREF "Page table for DST page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_PT_SURF_DST));
		printk(KERN_ERR PREF "Virt addr for DST page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_VADDR_SURF_DST));
	}
	if (mask & HARDDOOM_INTR_PAGE_FAULT_SURF_SRC) {
		printk(KERN_ERR PREF "Page table for SRC page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_PT_SURF_SRC));
		printk(KERN_ERR PREF "Virt addr for SRC page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_VADDR_SURF_SRC));
	}
	if (mask & HARDDOOM_INTR_PAGE_FAULT_TEXTURE) {
		printk(KERN_ERR PREF "Page table for TXT page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_PT_TEXTURE));
		printk(KERN_ERR PREF "Virt addr for TXT page fault: %08x",
		       ioread32(bar_ptr + HARDDOOM_TLB_VADDR_TEXTURE));
	}
}
