#include <linux/cdev.h>
#include <linux/dmapool.h>
#include <linux/fs.h>
#include <linux/interrupt.h>
#include <linux/module.h>
#include <linux/pci.h>

#include "buffers.h"
#include "common.h"
#include "debug.h"
#include "doomcode.h"
#include "doomdev.h"
#include "harddoom.h"

MODULE_LICENSE("GPL");

static dev_t harddoom_major;

#define DOOMDEV_REPR "HardDoom(TM) device driver"
#define MAX_HARDDOOM_DEVICES 256
#define DOOMDEV_MMIO_SIZE 4096

// -- BASIC HELPERS -- //

DEFINE_MUTEX(dev_lookup_mutex);

static int harddoom_dev_init(struct harddoom_dev *hddev)
{
	memset(hddev, 0, sizeof(struct harddoom_dev));
	INIT_LIST_HEAD(&hddev->open_descriptors);
	mutex_init(&hddev->dev_mutex);
	init_waitqueue_head(&hddev->dev_queue);
	INIT_LIST_HEAD(&hddev->writer.unsafe_surfaces);
	INIT_LIST_HEAD(&hddev->list_buffers_release);
	return 0;
}

static void harddoom_dev_destroy(struct harddoom_dev *hddev)
{
	mutex_destroy(&hddev->dev_mutex);
}

static struct harddoom_dev harddoom_devs[MAX_HARDDOOM_DEVICES];

static struct class harddoom_class = {
    .name = "doom",
    .owner = THIS_MODULE,
};

// -- /DEV/DOOM[XYZ] FUNCTIONALITY -- //

struct harddoom_inode_data *
create_harddoom_inode_data(struct harddoom_dev *hddev)
{
	struct harddoom_inode_data *data =
	    kmalloc(sizeof(struct harddoom_inode_data), GFP_KERNEL);

	if (unlikely(!data))
		return ERR_PTR(-ENOMEM);

	INIT_LIST_HEAD(&data->list_buffers);
	data->hddev = hddev;
	mutex_init(&data->inode_mutex);
	return data;
}

static void release_harddom_inode_data(struct harddoom_inode_data *data)
{
	mutex_destroy(&data->inode_mutex);
	kfree(data);
}

static int harddoom_open(struct inode *ino, struct file *filep)
{
	// Step by step:
	//  1. Fetch HardDoom device info from inode.
	//  2. Create an (empty) list of buffers to this descriptor.
	//  3. Add yourself to the global list of open descriptors.
	struct cdev *inode_cdev;
	struct harddoom_dev *hddev;
	struct harddoom_inode_data *inode_data;
	int err;

	inode_cdev = ino->i_cdev;

	hddev = container_of(inode_cdev, struct harddoom_dev, cdev);
	printk(KERN_INFO "Opening descriptor %s", hddev->userdev->kobj.name);

	inode_data = create_harddoom_inode_data(hddev);
	if (IS_ERR(inode_data))
		return PTR_ERR(inode_data);

	// Push yourself to the device's list of open descriptors.
	if (unlikely(err = mutex_lock_interruptible(&hddev->dev_mutex)))
		goto err_mutex;
	list_add(&inode_data->list_inodes, &hddev->open_descriptors);
	mutex_unlock(&hddev->dev_mutex);

	filep->private_data = (void *)inode_data;
	return 0;

err_mutex:
	release_harddom_inode_data(inode_data);
	return err;
}

static int harddoom_release(struct inode *ino, struct file *filep)
{
	struct harddoom_dev *hddev;
	struct harddoom_inode_data *inode_data;
	struct list_head *elem;

	inode_data = (struct harddoom_inode_data *)filep->private_data;
	hddev = inode_data->hddev;

	printk(KERN_INFO "Releasing descriptor %s", hddev->userdev->kobj.name);

	mutex_lock(&inode_data->inode_mutex);
	list_for_each(elem, &inode_data->list_buffers)
	{
		struct doomdev_buffer_header *bufh = container_of(
		    elem, struct doomdev_buffer_header, elem_buffers);
		harddoom_release_buffer(bufh);
	}

	// Mark the buffers to be released at next sync/device destruction,
	// and remove itself from the list of active inodes.
	mutex_lock(&hddev->dev_mutex);
	list_splice(&inode_data->list_buffers, &hddev->list_buffers_release);
	list_del(&inode_data->list_inodes);
	mutex_unlock(&hddev->dev_mutex);
	mutex_unlock(&inode_data->inode_mutex);

	return 0;
}

static long harddoom_ioctl(struct file *filep, unsigned int cmd,
			   unsigned long user_arg)
{
	void *arg;
	int err;
	switch (cmd) {
	case DOOMDEV_IOCTL_CREATE_SURFACE:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_ioctl_create_surface));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = harddoom_create_surface(filep->private_data, arg);
		kfree(arg);
		break;

	case DOOMDEV_IOCTL_CREATE_TEXTURE:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_ioctl_create_texture));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = harddoom_create_texture(filep->private_data, arg);
		kfree(arg);
		break;

	case DOOMDEV_IOCTL_CREATE_FLAT:
		arg = alloc_from_user((char __user *)user_arg,
				      sizeof(struct doomdev_ioctl_create_flat));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = harddoom_create_flat(filep->private_data, arg);
		kfree(arg);
		break;

	case DOOMDEV_IOCTL_CREATE_COLORMAPS:
		arg = alloc_from_user(
		    (char __user *)user_arg,
		    sizeof(struct doomdev_ioctl_create_colormaps));
		if (IS_ERR(arg))
			return PTR_ERR(arg);
		err = harddoom_create_colormaps(filep->private_data, arg);
		kfree(arg);
		break;

	default:
		err = -ENOTTY;
	}

	return err;
}

// -- DEVICE ALLOCATION -- //

static struct file_operations harddoom_file_ops = {
    .owner = THIS_MODULE,
    .open = harddoom_open,
    .unlocked_ioctl = harddoom_ioctl,
    .compat_ioctl = harddoom_ioctl,
    .release = harddoom_release,
};

const struct pci_device_id harddoom_device_id = {.vendor = 0x0666,
						 .device = 0x1993,
						 .subvendor = PCI_ANY_ID,
						 .subdevice = PCI_ANY_ID,
						 .class = 0,
						 .class_mask = 0};

static int probe_alloc_location(struct pci_dev *dev)
{
	int dev_idx;
	int err;
	if ((err = mutex_lock_interruptible(&dev_lookup_mutex)))
		return err;

	for (dev_idx = 0; dev_idx < MAX_HARDDOOM_DEVICES; ++dev_idx) {
		if (unlikely(harddoom_devs[dev_idx].pdev == dev)) {
			printk(KERN_WARNING
			       "HardDoom device already known: /dev/doom%d",
			       dev_idx);
			err = -EEXIST;
			goto end_alloc;
		}
		if (harddoom_devs[dev_idx].pdev != NULL)
			continue;

		printk(KERN_INFO "Found a free slot for HardDoom: /dev/doom%d",
		       dev_idx);
		err = dev_idx;
		harddoom_devs[dev_idx].pdev = dev;
		goto end_alloc;
	}
	printk(KERN_WARNING "No free slots for any new HardDoom device");
	err = -ENOMEM;

end_alloc:
	mutex_unlock(&dev_lookup_mutex);
	return err;
}

static void probe_free_location(struct harddoom_dev *hddev)
{
	mutex_lock(&dev_lookup_mutex);
	hddev->pdev = NULL;
	mutex_unlock(&dev_lookup_mutex);
}

// -- START/STOP CONVERSATION -- //

static int start_doomdev(struct harddoom_dev *hddev)
{
	void __iomem *ioptr = hddev->bar_iomap;
	int code_idx;
	// Write 0 to FE_CODE_ADDR.
	iowrite32(0, ioptr + HARDDOOM_FE_CODE_ADDR);
	// Write the doomcode.
	for (code_idx = 0; code_idx < doomcode_size; ++code_idx)
		iowrite32(doomcode[code_idx], ioptr + HARDDOOM_FE_CODE_WINDOW);
	// Reset the blocks.
	iowrite32(HARDDOOM_RESET_ALL, ioptr + HARDDOOM_RESET);
	// Clear and start interrupts.
	iowrite32(HARDDOOM_INTR_MASK, ioptr + HARDDOOM_INTR);
	iowrite32(HARDDOOM_INTR_MASK ^ HARDDOOM_INTR_PONG_ASYNC,
		  ioptr + HARDDOOM_INTR_ENABLE);
	// Initialize FENCE.
	iowrite32(0, ioptr + HARDDOOM_FENCE_LAST);
	iowrite32(HARDDOOM_FENCE_NO_VALUE, ioptr + HARDDOOM_FENCE_WAIT);
	// Enable everything except command-reading module.
	iowrite32(HARDDOOM_ENABLE_ALL ^ HARDDOOM_ENABLE_FETCH_CMD,
		  ioptr + HARDDOOM_ENABLE);

	return 0;
}

static void stop_doomdev(struct harddoom_dev *hddev)
{
	void __iomem *ioptr = hddev->bar_iomap;

	// Disable the blocks and interrupts. Then do an arbitrary read.
	iowrite32(0, ioptr + HARDDOOM_ENABLE);
	iowrite32(0, ioptr + HARDDOOM_INTR_ENABLE);
	ioread32(ioptr);
}

// -- INTERRUPTS -- //

static void harddoom_do_irq_tasklet(unsigned long data)
{
	struct harddoom_dev *hddev = (struct harddoom_dev *)data;

	spin_lock_irq(&hddev->irq_spinlock);
	harddoom_bugcheck(hddev);
	wake_up_interruptible(&hddev->dev_queue);
	spin_unlock_irq(&hddev->irq_spinlock);
}

// Interrupt handler. Should not do any sophisticated things.
static irqreturn_t harddoom_irq_handler(int irq, void *dev)
{
	struct harddoom_dev *hddev = (struct harddoom_dev *)dev;
	void __iomem *bar_intr_ptr = hddev->bar_iomap + HARDDOOM_INTR;
	u32 mask;

	// Make sure we're using the correct interrupt.
	if (hddev->pdev->irq != irq)
		return IRQ_NONE;

	// Check if we have any interrupts at all.
	mask = ioread32(bar_intr_ptr);
	if (!mask)
		return IRQ_NONE;
	// Clear these interrupts and save them locally.
	iowrite32(mask, bar_intr_ptr);
	atomic_or(mask, &hddev->interrupts);

	tasklet_schedule(&hddev->irq_tasklet);
	return IRQ_HANDLED;
}

// -- INIT, DESTRUCT -- //

static int do_probe(struct pci_dev *dev, const struct pci_device_id *id)
{
	int dev_idx, err, devt;
	struct harddoom_dev *hddev;

	printk(KERN_INFO "Probing a HardDoom device: %p", dev);
	dev_idx = probe_alloc_location(dev);
	if (unlikely(dev_idx < 0))
		return dev_idx;

	hddev = &harddoom_devs[dev_idx];

	if (unlikely(err = harddoom_dev_init(hddev)))
		goto err_dev_init;
	hddev->pdev = dev;

	cdev_init(&hddev->cdev, &harddoom_file_ops);
	devt = harddoom_major + dev_idx;
	if (unlikely(err = cdev_add(&hddev->cdev, devt, 1)))
		goto err_cdev;

	hddev->userdev =
	    device_create(&harddoom_class, NULL, devt, NULL, "doom%d", dev_idx);
	if (IS_ERR(hddev->userdev)) {
		err = PTR_ERR(hddev->userdev);
		goto err_create;
	}

	printk(KERN_INFO "Creating a HardDoom device at /dev/doom%d", dev_idx);

	hddev->dev_dma_pool = dma_pool_create(
	    hddev->userdev->kobj.name, &hddev->pdev->dev, HARDDOOM_PAGE_SIZE,
	    HARDDOOM_PAGE_SIZE, HARDDOOM_PAGE_SIZE);
	if (IS_ERR_OR_NULL(hddev->dev_dma_pool)) {
		err = IS_ERR(hddev->dev_dma_pool) ? PTR_ERR(hddev->dev_dma_pool)
						  : -ENOMEM;
		goto err_dma_pool;
	}

	if (unlikely(err = pci_enable_device(dev)))
		goto err_pci_enable;

	if (unlikely(err = pci_request_regions(dev, DOOMDEV_REPR)))
		goto err_regions;

	hddev->bar_iomap = pci_iomap(dev, 0, DOOMDEV_MMIO_SIZE);
	if (IS_ERR(hddev->bar_iomap)) {
		err = PTR_ERR(hddev->bar_iomap);
		goto err_iomap;
	}

	pci_set_master(hddev->pdev);

	if (unlikely(err = pci_set_dma_mask(hddev->pdev, DMA_BIT_MASK(32))))
		goto err_dma;

	if (unlikely(err = pci_set_consistent_dma_mask(hddev->pdev,
						       DMA_BIT_MASK(32))))
		goto err_dma;

	tasklet_init(&hddev->irq_tasklet, harddoom_do_irq_tasklet,
		     (unsigned long)hddev);
	spin_lock_init(&hddev->irq_spinlock);
	if (unlikely(err = request_irq(dev->irq, harddoom_irq_handler,
				       IRQF_SHARED, hddev->userdev->kobj.name,
				       hddev)))
		goto err_request_irq;

	if (unlikely(err = start_doomdev(hddev)))
		goto err_doomdev_start;

	hddev->pdev = dev;
	printk(KERN_INFO "Created a HardDoom device at /dev/doom%d", dev_idx);
	return 0;

err_doomdev_start:
	free_irq(dev->irq, hddev);
err_request_irq:
	tasklet_kill(&hddev->irq_tasklet);
err_dma:
	pci_iounmap(dev, harddoom_devs[dev_idx].bar_iomap);
	pci_clear_master(harddoom_devs[dev_idx].pdev);
err_iomap:
	pci_release_regions(dev);
err_regions:
	pci_disable_device(dev);
err_pci_enable:
	dma_pool_destroy(hddev->dev_dma_pool);
err_dma_pool:
	device_destroy(&harddoom_class, harddoom_major + dev_idx);
err_create:
	cdev_del(&harddoom_devs[dev_idx].cdev);
err_cdev:
	harddoom_devs[dev_idx].pdev = NULL;
	harddoom_dev_destroy(hddev);
err_dev_init:
	probe_free_location(hddev);
	return err;
}

static void do_remove(struct pci_dev *dev)
{
	struct harddoom_dev *hddev;
	int dev_idx;

	printk(KERN_INFO "Removing a HardDoom device: %p", dev);
	mutex_lock(&dev_lookup_mutex);
	for (dev_idx = 0; dev_idx < MAX_HARDDOOM_DEVICES; ++dev_idx) {
		if (harddoom_devs[dev_idx].pdev == dev)
			break;
	}
	mutex_unlock(&dev_lookup_mutex);

	if (unlikely(dev_idx == MAX_HARDDOOM_DEVICES)) {
		printk(KERN_WARNING "Couldn't find HardDoom device: %p", dev);
		return;
	}

	hddev = &harddoom_devs[dev_idx];

	printk(KERN_INFO "Removing a HardDoom device at /dev/doom%d", dev_idx);
	stop_doomdev(hddev);
	release_marked_buffers(hddev);
	free_irq(dev->irq, hddev);
	tasklet_kill(&hddev->irq_tasklet);
	pci_iounmap(dev, hddev->bar_iomap);
	pci_release_regions(dev);
	pci_disable_device(dev);
	device_destroy(&harddoom_class, harddoom_major + dev_idx);
	dma_pool_destroy(hddev->dev_dma_pool);
	cdev_del(&hddev->cdev);
	harddoom_dev_destroy(hddev);
	probe_free_location(hddev);
	printk(KERN_INFO "Removed a HardDoom device at /dev/doom%d", dev_idx);
}

static struct pci_driver harddoom_driver = {
    .name = "Graphics accelerator: HardDoom(TM)",
    .id_table = &harddoom_device_id,
    .probe = do_probe,
    .remove = do_remove,
};

static int harddoom_init(void)
{
	int err;
	printk(KERN_INFO "HardDoom driver initialization started");

	if (unlikely(err = alloc_chrdev_region(&harddoom_major, 0,
					       MAX_HARDDOOM_DEVICES, "doom")))
		goto err_alloc_region;
	if (unlikely(err = class_register(&harddoom_class)))
		goto err_class;
	if (unlikely(err = pci_register_driver(&harddoom_driver)))
		goto err_reg_driver;

	printk(KERN_INFO "HardDoom driver initialized");

	return 0;

err_reg_driver:
	class_unregister(&harddoom_class);
err_class:
	unregister_chrdev_region(harddoom_major, MAX_HARDDOOM_DEVICES);
err_alloc_region:
	return err;
}

static void harddoom_cleanup(void)
{
	printk(KERN_INFO "HardDoom driver unloading requested");

	pci_unregister_driver(&harddoom_driver);
	class_unregister(&harddoom_class);
	unregister_chrdev_region(harddoom_major, MAX_HARDDOOM_DEVICES);

	printk(KERN_INFO "Goodbye!");
}

// -- MODULE INFO -- //

module_init(harddoom_init);
module_exit(harddoom_cleanup);
