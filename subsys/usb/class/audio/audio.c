/*
 * Copyright (c) 2020 Nordic Semiconductor ASA
 *
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @file
 * @brief Audio device class driver
 *
 * Driver for USB Audio device class driver
 */

#include <kernel.h>
#include <usb/usb_common.h>
#include <usb/usb_device.h>
#include <usb_descriptor.h>
#include <usb/usbstruct.h>
#include <usb/class/usb_audio.h>
#include "usb_audio_internal.h"

#include <sys/byteorder.h>
#include <net/buf.h>

#include <logging/log.h>
LOG_MODULE_REGISTER(usb_audio, CONFIG_USB_AUDIO_LOG_LEVEL);

/* Device data structure */
struct usb_audio_dev_data_t {
	const struct usb_audio_ops *ops;

	struct usb_audio_features *controls[2];

	const struct cs_ac_interface_descriptor_header *header_descr;

	struct usb_dev_data common;

	struct net_buf_pool *pool;

	/* Not applicable for Headphones, left with 0 */
	u16_t in_frame_size;

	bool rx_enable;
	bool tx_enable;
};

static sys_slist_t usb_audio_data_devlist;

/**
 * @brief Fill the USB Audio descriptor
 *
 * This macro fills USB descriptor for specific type of device
 * (Heahphones or Microphone) depending on dev param.
 *
 * @note Feature unit has variable length and only 1st field of
 *	 .bmaControls is filled. Later its fixed in usb_fix_descriptor()
 * @note Audio control and Audio streaming interfaces are numerated starting
 *	 from 0 and are later fixed in usb_fix_descriptor()
 *
 * @param [in] dev	Device type. Must be HP/MIC
 * @param [in] i	Instance of device of current type (dev)
 * @param [in] id	Param for counting logic entities
 * @param [in] link	ID of IN/OUT terminal to which General Descriptor
 *			is linked.
 * @param [in] it_type	Input terminal type
 * @param [in] ot_type	Output terminal type
 */
#define DEFINE_AUDIO_DESCRIPTOR(dev, i, id, link, it_type, ot_type, cb, addr) \
USBD_CLASS_DESCR_DEFINE(primary, audio)					      \
struct dev##_descriptor_##i dev##_desc_##i = {				      \
	USB_AUDIO_IAD							      \
	.std_ac_interface = INIT_STD_IF(USB_AUDIO_AUDIOCONTROL, 0, 0, 0),     \
	.ac_interface_header = INIT_CS_AC_IF_HEADER(dev, i, 1),		      \
	.input_terminal = INIT_IN_TERMINAL(dev, i, id, it_type),	      \
	.feature_unit = INIT_FEATURE_UNIT(dev, i, id + 1, id),		      \
	.output_terminal = INIT_OUT_TERMINAL(id + 2, id + 1, ot_type),	      \
	.as_interface_alt_0 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING, 1, 0, 0), \
	.as_interface_alt_1 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING, 1, 1, 1), \
	.as_cs_interface = INIT_AS_GENERAL(link),			      \
	.format = INIT_AS_FORMAT_I(CH_CNT(dev, i), GET_RES(dev, i)),	      \
	.std_ep_desc = INIT_STD_AS_AD_EP(dev, i, addr),			      \
	.cs_ep_desc = INIT_CS_AS_AD_EP,					      \
};									      \
static struct usb_ep_cfg_data dev##_usb_audio_ep_data_##i[] = {		      \
	INIT_EP_DATA(cb, addr),						      \
}

/**
 * @brief Fill the USB Audio descriptor
 *
 * This macro fills USB descriptor for specific type of device.
 * Macro is used when the device uses 2 audiostreaming interfaces,
 * eg. Headset
 *
 * @note Feature units have variable length and only 1st field of
 *	 .bmaControls is filled. Its fixed in usb_fix_descriptor()
 * @note Audio control and Audio streaming interfaces are numerated starting
 *	 from 0 and are later fixed in usb_fix_descriptor()
 *
 * @param [in] dev	Device type.
 * @param [in] i	Instance of device of current type (dev)
 * @param [in] id	Param for counting logic entities
 */
#define DEFINE_AUDIO_DESCRIPTOR_BIDIR(dev, i, id)			  \
USBD_CLASS_DESCR_DEFINE(primary, audio)					  \
struct dev##_descriptor_##i dev##_desc_##i = {				  \
	USB_AUDIO_IAD							  \
	.std_ac_interface = INIT_STD_IF(USB_AUDIO_AUDIOCONTROL, 0, 0, 0), \
	.ac_interface_header = INIT_CS_AC_IF_HEADER_BIDIR(dev, i, 2),	  \
	.input_terminal_0 = INIT_IN_TERMINAL(dev##_MIC, i, id,		  \
						USB_AUDIO_IO_HEADSET),	  \
	.feature_unit_0 = INIT_FEATURE_UNIT(dev##_MIC, i, id+1, id),	  \
	.output_terminal_0 = INIT_OUT_TERMINAL(id+2, id+1,		  \
					USB_AUDIO_USB_STREAMING),	  \
	.input_terminal_1 = INIT_IN_TERMINAL(dev##_HP, i, id+3,		  \
					USB_AUDIO_USB_STREAMING),	  \
	.feature_unit_1 = INIT_FEATURE_UNIT(dev##_HP, i, id+4, id+3),	  \
	.output_terminal_1 = INIT_OUT_TERMINAL(id+5, id+4,		  \
						USB_AUDIO_IO_HEADSET),	  \
	.as_interface_alt_0_0 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING,	  \
						1, 0, 0),		  \
	.as_interface_alt_0_1 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING,	  \
						1, 1, 1),		  \
		.as_cs_interface_0 = INIT_AS_GENERAL(id+2),		  \
		.format_0 = INIT_AS_FORMAT_I(CH_CNT(dev##_MIC, i),	  \
					     GET_RES(dev##_MIC, i)),	  \
		.std_ep_desc_0 = INIT_STD_AS_AD_EP(dev##_MIC, i,	  \
						   AUTO_EP_IN),		  \
		.cs_ep_desc_0 = INIT_CS_AS_AD_EP,			  \
	.as_interface_alt_1_0 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING,	  \
						2, 0, 0),		  \
	.as_interface_alt_1_1 = INIT_STD_IF(USB_AUDIO_AUDIOSTREAMING,	  \
						2, 1, 1),		  \
		.as_cs_interface_1 = INIT_AS_GENERAL(id+3),		  \
		.format_1 = INIT_AS_FORMAT_I(CH_CNT(dev##_HP, i),	  \
					     GET_RES(dev##_HP, i)),	  \
		.std_ep_desc_1 = INIT_STD_AS_AD_EP(dev##_HP, i,		  \
						   AUTO_EP_OUT),	  \
		.cs_ep_desc_1 = INIT_CS_AS_AD_EP,			  \
};									  \
static struct usb_ep_cfg_data dev##_usb_audio_ep_data_##i[] = {		  \
	INIT_EP_DATA(usb_transfer_ep_callback, AUTO_EP_IN),		  \
	INIT_EP_DATA(audio_receive_cb, AUTO_EP_OUT),			  \
}

#define DEFINE_AUDIO_DEV_DATA(dev, i, __out_pool, __in_pool_size)	     \
	static struct usb_audio_features dev##_ctrls_##i[CH_CNT(dev, i) + 1];\
	static struct usb_audio_dev_data_t dev##_audio_dev_data_##i =	     \
		{ .controls = {dev##_ctrls_##i, NULL},			     \
		  .pool = __out_pool,					     \
		  .in_frame_size = __in_pool_size }

#define DEFINE_AUDIO_DEV_DATA_BIDIR(dev, i, __out_pool, __in_pool_size)	      \
	static struct usb_audio_features dev##_ctrls0_##i[CH_CNT(dev, i) + 1];\
	static struct usb_audio_features dev##_ctrls1_##i[CH_CNT(dev, i) + 1];\
	static struct usb_audio_dev_data_t dev##_audio_dev_data_##i =	      \
		{ .controls = {dev##_ctrls0_##i, dev##_ctrls1_##i},	      \
		  .pool = __out_pool,					      \
		  .in_frame_size = __in_pool_size }

/**
 * Helper function for getting channel number directly from the
 * feature unit descriptor.
 */
static u8_t get_num_of_channels(struct feature_unit_descriptor *fu)
{
	return (fu->bLength - FU_FIXED_ELEMS_SIZE)/sizeof(u16_t);
}

/**
 * Helper function for getting supported controls directly from
 * the feature unit descriptor.
 */
static u16_t get_controls(struct feature_unit_descriptor *fu)
{
	return *(u16_t *)((u8_t *)fu + BMA_CONTROLS_OFFSET);
}

/**
 * Helper function for getting the device streaming direction
 */
static enum usb_audio_direction get_fu_dir(struct feature_unit_descriptor *fu)
{
	struct output_terminal_descriptor *ot  =
	(struct output_terminal_descriptor *)((u8_t *)fu + fu->bLength);
	enum usb_audio_direction dir;

	if (ot->wTerminalType == USB_AUDIO_USB_STREAMING) {
		dir = USB_AUDIO_IN;
	} else {
		dir = USB_AUDIO_OUT;
	}

	return dir;
}

/**
 * Helper function for fixing controls in feature units descriptors.
 */
static void fix_fu_descriptors(struct usb_if_descriptor *iface)
{
	struct cs_ac_interface_descriptor_header *header;
	struct feature_unit_descriptor *fu;

	header = (struct cs_ac_interface_descriptor_header *)
			((u8_t *)iface + USB_PASSIVE_IF_DESC_SIZE);

	fu = (struct feature_unit_descriptor *)((u8_t *)header +
						header->bLength +
						INPUT_TERMINAL_DESC_SIZE);

	/* start from 1 as elem 0 is filled when descriptor is declared */
	for (int i = 1; i < get_num_of_channels(fu); i++) {
		*(fu->bmaControls + i) = fu->bmaControls[0];
	}

	if (header->bInCollection == 2) {
		fu = (struct feature_unit_descriptor *)((u8_t *)fu +
			fu->bLength +
			INPUT_TERMINAL_DESC_SIZE +
			OUTPUT_TERMINAL_DESC_SIZE);
		for (int i = 1; i < get_num_of_channels(fu); i++) {
			*(fu->bmaControls + i) = fu->bmaControls[0];
		}
	}
}

/**
 * Helper function for getting pointer to feature unit descriptor.
 * This is needed in order to address audio specific requests to proper
 * controls struct.
 */
static int get_feature_unit(struct usb_audio_dev_data_t *audio_dev_data,
			    struct feature_unit_descriptor **fu, u8_t fu_id)
{
	*fu = (struct feature_unit_descriptor *)
		((u8_t *)audio_dev_data->header_descr +
		audio_dev_data->header_descr->bLength +
		INPUT_TERMINAL_DESC_SIZE);

	if ((*fu)->bUnitID == fu_id) {
		return 0;
	}
	/* skip to the next Feature Unit */
	*fu = (struct feature_unit_descriptor *)
		(u8_t *)((u8_t *)*fu + (*fu)->bLength +
			INPUT_TERMINAL_DESC_SIZE +
			OUTPUT_TERMINAL_DESC_SIZE);

	return 1;
}

/**
 * @brief This is a helper function user to inform the user about
 * possibility to write the data to the device.
 */
static void audio_dc_sof(struct usb_cfg_data *cfg,
			 struct usb_audio_dev_data_t *dev_data)
{
	u8_t ep_addr;

	for (int i = 0; i < cfg->num_endpoints; i++) {
		ep_addr = cfg->endpoint[0].ep_addr;
		if ((ep_addr & USB_EP_DIR_MASK) && (dev_data->tx_enable)) {
			if (dev_data->ops && dev_data->ops->data_request_cb) {
				dev_data->ops->data_request_cb(
					dev_data->common.dev);
			}
		}
	}
}

static void audio_interface_config(struct usb_desc_header *head,
				   u8_t bInterfaceNumber)
{
	struct usb_if_descriptor *iface = (struct usb_if_descriptor *)head;
	struct cs_ac_interface_descriptor_header *header;

#ifdef CONFIG_USB_COMPOSITE_DEVICE
	struct usb_association_descriptor *iad =
		(struct usb_association_descriptor *)
		((char *)iface - sizeof(struct usb_association_descriptor));
	iad->bFirstInterface = bInterfaceNumber;
#endif
	fix_fu_descriptors(iface);

	/* Audio Control Interface */
	iface->bInterfaceNumber = bInterfaceNumber;
	header = (struct cs_ac_interface_descriptor_header *)
		 ((u8_t *)iface + iface->bLength);
	header->baInterfaceNr[0] = bInterfaceNumber + 1;

	/* Audio Streaming Interface Passive */
	iface = (struct usb_if_descriptor *)
			  ((u8_t *)header + header->wTotalLength);
	iface->bInterfaceNumber = bInterfaceNumber + 1;

	/* Audio Streaming Interface Active */
	iface = (struct usb_if_descriptor *)
			  ((u8_t *)iface + iface->bLength);
	iface->bInterfaceNumber = bInterfaceNumber + 1;

	if (header->bInCollection == 2) {
		header->baInterfaceNr[1] = bInterfaceNumber + 2;
		/* Audio Streaming Interface Passive */
		iface = (struct usb_if_descriptor *)
			((u8_t *)iface + USB_ACTIVE_IF_DESC_SIZE);
		iface->bInterfaceNumber = bInterfaceNumber + 2;

		/* Audio Streaming Interface Active */
		iface = (struct usb_if_descriptor *)
			((u8_t *)iface + USB_PASSIVE_IF_DESC_SIZE);
		iface->bInterfaceNumber = bInterfaceNumber + 2;
	}
}

static void audio_cb_usb_status(struct usb_cfg_data *cfg,
			 enum usb_dc_status_code cb_status,
			 const u8_t *param)
{
	struct usb_audio_dev_data_t *audio_dev_data;
	struct usb_dev_data *dev_data;

	dev_data = usb_get_dev_data_by_cfg(&usb_audio_data_devlist, cfg);

	if (dev_data == NULL) {
		LOG_ERR("Device data not found for cfg %p", cfg);
		return;
	}

	audio_dev_data = CONTAINER_OF(dev_data, struct usb_audio_dev_data_t,
				      common);

	switch (cb_status) {
	case USB_DC_SOF:
		audio_dc_sof(cfg, audio_dev_data);
		break;
	default:
		break;
	}
}

/**
 * @brief Helper funciton for checking if particular entity is a part of
 *	  the audio device.
 *
 * This function checks if given entity is a part of given audio device.
 * If so then true is returned and audio_dev_data is considered correct device
 * data.
 *
 * @note For now this function searches through feature units only. The
 *	 descriptors are known and are not using any other entity type.
 *	 If there is a need to add other units to audio function then this
 *	 must be reworked.
 *
 * @param [in]      audio_dev_data USB audio device data.
 * @param [in, out] entity	   USB Audio entity.
 *				   .id      [in]  id of searched entity
 *				   .subtype [out] subtype of entity (if found)
 *
 * @return true if entity matched audio_dev_data, false otherwise.
 */
static bool is_entity_valid(struct usb_audio_dev_data_t *audio_dev_data,
			    struct usb_audio_entity *entity)
{
	const struct cs_ac_interface_descriptor_header *header;
	struct feature_unit_descriptor *fu;

	header = audio_dev_data->header_descr;
	fu = (struct feature_unit_descriptor *)((u8_t *)header +
						header->bLength +
						INPUT_TERMINAL_DESC_SIZE);
	if (fu->bUnitID == entity->id) {
		entity->subtype = fu->bDescriptorSubtype;
		return true;
	}

	if (header->bInCollection == 2) {
		fu = (struct feature_unit_descriptor *)((u8_t *)fu +
			fu->bLength +
			INPUT_TERMINAL_DESC_SIZE +
			OUTPUT_TERMINAL_DESC_SIZE);
		if (fu->bUnitID == entity->id) {
			entity->subtype = fu->bDescriptorSubtype;
			return true;
		}
	}

	return false;
}

/**
 * @brief Helper funciton for getting the audio_dev_data by the entity number.
 *
 * This function searches through all audio devices the one with given
 * entity number and return the audio_dev_data structure for this entity.
 *
 * @param [in, out] entity USB Audio entity addressed by the request.
 *			   .id      [in]  id of searched entity
 *			   .subtype [out] subtype of entity (if found)
 *
 * @return audio_dev_data for given entity, NULL if not found.
 */
static struct usb_audio_dev_data_t *get_audio_dev_data_by_entity(
					struct usb_audio_entity *entity)
{
	struct usb_dev_data *dev_data;
	struct usb_audio_dev_data_t *audio_dev_data;

	SYS_SLIST_FOR_EACH_CONTAINER(&usb_audio_data_devlist, dev_data, node) {
		audio_dev_data = CONTAINER_OF(dev_data,
					      struct usb_audio_dev_data_t,
					      common);

		if (is_entity_valid(audio_dev_data, entity)) {
			return audio_dev_data;
		}
	}
	return NULL;
}

/**
 * @brief Helper funciton for checking if particular interface is a part of
 *	  the audio device.
 *
 * This function checks if given interface is a part of given audio device.
 * If so then true is returned and audio_dev_data is considered correct device
 * data.
 *
 * @param [in] audio_dev_data USB audio device data.
 * @param [in] interface      USB Audio interface number.
 *
 * @return true if interface matched audio_dev_data, false otherwise.
 */
static bool is_interface_valid(struct usb_audio_dev_data_t *audio_dev_data,
			       u8_t interface)
{
	const struct cs_ac_interface_descriptor_header *header;

	header = audio_dev_data->header_descr;
	u8_t desc_iface = 0;

	for (size_t i = 0; i < header->bInCollection; i++) {
		desc_iface = header->baInterfaceNr[i];
		if (desc_iface == interface) {
			return true;
		}
	}

	return false;
}

/**
 * @brief Helper funciton for getting the audio_dev_data by the interface
 *	  number.
 *
 * This function searches through all audio devices the one with given
 * interface number and returns the audio_dev_data structure for this device.
 *
 * @param [in] interface USB Audio interface addressed by the request.
 *
 * @return audio_dev_data for given interface, NULL if not found.
 */
static struct usb_audio_dev_data_t *get_audio_dev_data_by_interface(
								u8_t interface)
{
	struct usb_dev_data *dev_data;
	struct usb_audio_dev_data_t *audio_dev_data;

	SYS_SLIST_FOR_EACH_CONTAINER(&usb_audio_data_devlist, dev_data, node) {
		audio_dev_data = CONTAINER_OF(dev_data,
					struct usb_audio_dev_data_t, common);

		if (is_interface_valid(audio_dev_data, interface)) {
			return audio_dev_data;
		}
	}
	return NULL;
}

/**
 * @brief Handler for feature unit requests.
 *
 * This function handles feature unit specific requests.
 * If request is properly served 0 is returned. Negative errno
 * is returned in case of an error. This leads to setting stall on IN EP0.
 *
 * @param [in]  dev_data USB audio device data.
 * @param [in]  pSetup   Information about the executed request.
 * @param [in]  len      Size of the buffer.
 * @param [out] data     Buffer containing the request result.
 *
 * @return 0 if succesfulf, negative errno otherwise.
 */
static int handle_feature_unit_req(struct usb_audio_dev_data_t *audio_dev_data,
				   struct usb_setup_packet *pSetup,
				   s32_t *len,
				   u8_t **data)
{
	enum usb_audio_feature_unit_control_selectors control_selector;
	struct feature_unit_descriptor *fu = NULL;
	struct usb_audio_feature_unit_evt evt;
	u8_t ch_num, ch_start, ch_end;
	static u8_t tmp_data_ptr[3];
	u8_t data_offset = 0;
	u8_t fu_id = ((pSetup->wIndex) >> 8) & 0xFF;
	u8_t device = get_feature_unit(audio_dev_data, &fu, fu_id);
	int ret = -EINVAL;

	evt.dir = get_fu_dir(fu);
	ch_num = (pSetup->wValue) & 0xFF;
	control_selector = ((pSetup->wValue) >> 8) & 0xFF;
	ch_start = ch_num == 0xFF ? 0 : ch_num;
	ch_end = ch_num == 0xFF ? get_num_of_channels(fu) : ch_num + 1;

	LOG_DBG("CS: %d, CN: %d, len: %d", control_selector, ch_num, *len);

	/* Error checking */
	if (!(BIT(control_selector) & (get_controls(fu) << 1UL))) {
		return -EINVAL;
	} else if (ch_num >= get_num_of_channels(fu) &&
		   ch_num != 0xFF) {
		return -EINVAL;
	}

	for (int ch = ch_start; ch < ch_end; ch++) {
		switch (control_selector) {
		case USB_AUDIO_FU_MUTE_CONTROL:
			switch (pSetup->bRequest) {
			case USB_AUDIO_SET_CUR:
				audio_dev_data->controls[device][ch].mute =
						((*data)[data_offset] != 0);
				/* Inform the user APP by callback */
				if (audio_dev_data->ops->feature_update_cb &&
				    audio_dev_data->ops) {
					evt.cs = control_selector;
					evt.channel = ch;
					evt.val = &audio_dev_data->controls[device][ch].mute;
					audio_dev_data->ops->feature_update_cb(audio_dev_data->common.dev, evt);
				}
				ret = 0;
				break;
			case USB_AUDIO_GET_CUR:
				tmp_data_ptr[data_offset] = (audio_dev_data->controls[device][ch].mute != 0);
				ret = 0;
				break;
			default:
				ret = -EINVAL;
			}
			data_offset++;
			break;
		case USB_AUDIO_FU_VOLUME_CONTROL:
		case USB_AUDIO_FU_BASS_CONTROL:
		case USB_AUDIO_FU_MID_CONTROL:
		case USB_AUDIO_FU_TREBLE_CONTROL:
		case USB_AUDIO_FU_GRAPHIC_EQUALIZER_CONTROL:
		case USB_AUDIO_FU_AUTOMATIC_GAIN_CONTROL:
		case USB_AUDIO_FU_DELAY_CONTROL:
		case USB_AUDIO_FU_BASS_BOOST_CONTROL:
		case USB_AUDIO_FU_LOUDNESS_CONTROL:
			break;
		default:
			break;
		}
	}

	/* Process IN request */
	if (REQTYPE_GET_DIR(pSetup->bmRequestType) == REQTYPE_DIR_TO_HOST) {
		*data = tmp_data_ptr;
		*len = data_offset;
	}

	return ret;
}

/**
 * @brief Handler called for class specific interface request.
 *
 * This function handles all class specific interface requests to a usb audio
 * device. If request is properly server then 0 is returned. Returning negative
 * value will lead to set stall on IN EP0.
 *
 * @param pSetup    Information about the executed request.
 * @param len       Size of the buffer.
 * @param data      Buffer containing the request result.
 *
 * @return  0 on success, negative errno code on fail.
 */
static int handle_interface_req(struct usb_setup_packet *pSetup,
				s32_t *len,
				u8_t **data)
{
	struct usb_audio_dev_data_t *audio_dev_data;
	struct usb_audio_entity entity;

	/* parse wIndex for interface request */
	u8_t entity_id = ((pSetup->wIndex) >> 8) & 0xFF;

	entity.id = entity_id;

	/** Normally there should be a call to usb_get_dev_data_by_iface()
	 * and addressed interface should be read from wIndex low byte.
	 *
	 * u8_t interface = (pSetup->wIndex) & 0xFF;
	 *
	 * However, Linux is using special form of Audio Requests
	 * which always left wIndex low byte 0 no matter which device and
	 * entity is addressed. Because of that there is a need to obtain
	 * this information from the device descriptor using entity id.
	 */
	audio_dev_data = get_audio_dev_data_by_entity(&entity);

	if (audio_dev_data == NULL) {
		LOG_ERR("Device data not found for entity %u", entity.id);
		return -ENODEV;
	}

	switch (entity.subtype) {
	case USB_AUDIO_FEATURE_UNIT:
		return handle_feature_unit_req(audio_dev_data,
					       pSetup, len, data);
	case USB_AUDIO_INPUT_TERMINAL:
	case USB_AUDIO_OUTPUT_TERMINAL:
	case USB_AUDIO_MIXER_UNIT:
	case USB_AUDIO_SELECTOR_UNIT:
	case USB_AUDIO_PROCESSING_UNIT:
	case USB_AUDIO_EXTENSION_UNIT:
	default:
		LOG_INF("Currently not supported");
		return -ENODEV;
	}

	return 0;
}

/**
 * @brief Handler called for class specific endpoint request.
 *
 * This function handles all class specific endpoint requests to a usb audio
 * device. If request is properly server then 0 is returned. Returning negative
 * value will lead to set stall on IN EP0.
 *
 * @param pSetup    Information about the executed request.
 * @param len       Size of the buffer.
 * @param data      Buffer containing the request result.
 *
 * @return  0 on success, negative errno code on fail.
 */
static int handle_endpoint_req(struct usb_setup_packet *pSetup,
				      s32_t *len,
				      u8_t **data)
{
	return -1;
}

/**
 * @brief Custom callback for USB Device requests.
 *
 * This callback is called when set/get interface request is directed
 * to the device. This is Zephyr way to address those requests.
 * It's not possible to do that in the core stack as common USB device
 * stack does not know the amount of devices that has alternate interfaces.
 *
 * @param pSetup    Information about the request to execute.
 * @param len       Size of the buffer.
 * @param data      Buffer containing the request result.
 *
 * @return 0 on success, positive value if request is intended to be handled
 *	   by the core USB stack. Negative error code on fail.
 */
static int audio_custom_handler(struct usb_setup_packet *pSetup,
				s32_t *len, u8_t **data)
{
	const struct cs_ac_interface_descriptor_header *header;
	struct usb_audio_dev_data_t *audio_dev_data;
	struct usb_if_descriptor *if_desc;
	struct usb_ep_descriptor *ep_desc;

	u8_t iface = (pSetup->wIndex) & 0xFF;

	audio_dev_data = get_audio_dev_data_by_interface(iface);
	if (audio_dev_data == NULL) {
		return -ENOTSUP;
	}

	/* Search for endpoint associated to addressed interface
	 * Endpoint is searched in order to know the direction of
	 * addressed interface.
	 */
	header = audio_dev_data->header_descr;

	/* Skip to the first interface */
	if_desc = (struct usb_if_descriptor *)((u8_t *)header +
						header->wTotalLength +
						USB_PASSIVE_IF_DESC_SIZE);

	if (if_desc->bInterfaceNumber == iface) {
		ep_desc = (struct usb_ep_descriptor *)((u8_t *)if_desc +
						USB_PASSIVE_IF_DESC_SIZE +
						USB_AC_CS_IF_DESC_SIZE +
						USB_FORMAT_TYPE_I_DESC_SIZE);
	} else {
		/* In case first interface address is not the one addressed
		 * we can be sure the second one is because
		 * get_audio_dev_data_by_interface() found the device. It
		 * must be the second interface associated with the device.
		 */
		if_desc = (struct usb_if_descriptor *)((u8_t *)if_desc +
						USB_ACTIVE_IF_DESC_SIZE);
		ep_desc = (struct usb_ep_descriptor *)((u8_t *)if_desc +
						USB_PASSIVE_IF_DESC_SIZE +
						USB_AC_CS_IF_DESC_SIZE +
						USB_FORMAT_TYPE_I_DESC_SIZE);
	}

	if (REQTYPE_GET_RECIP(pSetup->bmRequestType) ==
	    REQTYPE_RECIP_INTERFACE) {
		switch (pSetup->bRequest) {
		case REQ_SET_INTERFACE:
			if (ep_desc->bEndpointAddress & USB_EP_DIR_MASK) {
				audio_dev_data->tx_enable = pSetup->wValue;
			} else {
				audio_dev_data->rx_enable = pSetup->wValue;
			}
			return 1;
		case REQ_GET_INTERFACE:
			if (ep_desc->bEndpointAddress & USB_EP_DIR_MASK) {
				*data[0] = audio_dev_data->tx_enable;
			} else {
				*data[0] = audio_dev_data->rx_enable;
			}
			return 0;
		default:
			break;
		}
	}

	return -ENOTSUP;
}

/**
 * @brief Handler called for Class requests not handled by the USB stack.
 *
 * @param pSetup    Information about the request to execute.
 * @param len       Size of the buffer.
 * @param data      Buffer containing the request result.
 *
 * @return  0 on success, negative errno code on fail.
 */
static int audio_class_handle_req(struct usb_setup_packet *pSetup,
				  s32_t *len, u8_t **data)
{
	LOG_INF("bmRequestType 0x%02x, bRequest 0x%02x, wValue 0x%04x,"
		"wIndex 0x%04x, wLength 0x%04x",
		pSetup->bmRequestType, pSetup->bRequest, pSetup->wValue,
		pSetup->wIndex, pSetup->wLength);

	switch (REQTYPE_GET_RECIP(pSetup->bmRequestType)) {
	case REQTYPE_RECIP_INTERFACE:
		return handle_interface_req(pSetup, len, data);
	case REQTYPE_RECIP_ENDPOINT:
		return handle_endpoint_req(pSetup, len, data);
	default:
		LOG_ERR("Request receipent invalid");
		return -1;
	}
}

static int usb_audio_device_init(struct device *dev)
{
	LOG_DBG("Init Audio Device: dev %p (%s)", dev, dev->config->name);

	return 0;
}

static void audio_write_cb(u8_t ep, int size, void *priv)
{
	struct usb_dev_data *dev_data;
	struct usb_audio_dev_data_t *audio_dev_data;
	struct net_buf *buffer = priv;

	dev_data = usb_get_dev_data_by_ep(&usb_audio_data_devlist, ep);
	audio_dev_data = dev_data->dev->driver_data;

	LOG_DBG("Written %d bytes on ep 0x%02x, *audio_dev_data %p",
		size, ep, audio_dev_data);

	/* Release net_buf back to the pool */
	net_buf_unref(buffer);

	/* Call user callback if user registered one */
	if (audio_dev_data->ops && audio_dev_data->ops->data_written_cb) {
		audio_dev_data->ops->data_written_cb(dev_data->dev, NULL, size);
	}
}

int usb_audio_send(const struct device *dev, struct net_buf *buffer,
		   size_t len)
{
	struct usb_audio_dev_data_t *audio_dev_data = dev->driver_data;
	struct usb_cfg_data *cfg = (void *)dev->config->config_info;
	/* EP ISO IN is always placed first in the endpoint table */
	u8_t ep = cfg->endpoint[0].ep_addr;

	if (!(ep & USB_EP_DIR_MASK)) {
		LOG_ERR("Wrong device");
		return -EINVAL;
	}

	if (!audio_dev_data->tx_enable) {
		LOG_DBG("sending dropped -> Host chose passive interface");
		return -EAGAIN;
	}

	if (len > buffer->size) {
		LOG_ERR("Cannot send %d bytes, to much data", len);
		return -EINVAL;
	}

	/** buffer passed to *priv because completion callback
	 * needs to release it to the pool
	 */
	usb_transfer(ep, buffer->data, len, USB_TRANS_WRITE | USB_TRANS_NO_ZLP,
		     audio_write_cb, buffer);
	return 0;
}

size_t usb_audio_get_in_frame_size(const struct device *dev)
{
	struct usb_audio_dev_data_t *audio_dev_data = dev->driver_data;

	return audio_dev_data->in_frame_size;
}

static void audio_receive_cb(u8_t ep, enum usb_dc_ep_cb_status_code status)
{
	struct usb_audio_dev_data_t *audio_dev_data;
	struct usb_dev_data *common;
	struct net_buf *buffer;
	int ret_bytes;
	int ret;

	__ASSERT(status == USB_DC_EP_DATA_OUT, "Invalid ep status");

	common = usb_get_dev_data_by_ep(&usb_audio_data_devlist, ep);
	if (common == NULL) {
		return;
	}

	audio_dev_data = CONTAINER_OF(common,
				      struct usb_audio_dev_data_t,
				      common);

	/** Check if active audiostreaming interface is selected
	 * If no there is no point to read the data. Return from callback
	 */
	if (!audio_dev_data->rx_enable) {
		return;
	}

	buffer = net_buf_alloc(audio_dev_data->pool, K_NO_WAIT);
	if (!buffer) {
		LOG_ERR("Failed to allocate data buffer");
		return;
	}

	ret = usb_read(ep, buffer->data, buffer->size, &ret_bytes);

	if (ret) {
		LOG_ERR("ret=%d ", ret);
		net_buf_unref(buffer);
		return;
	}

	if (!ret_bytes) {
		net_buf_unref(buffer);
		return;
	}

	/* Ask installed callback to process the data.
	 * User is responsible for freeing the buffer.
	 * In case no callback is installed free the buffer.
	 */
	if (audio_dev_data->ops && audio_dev_data->ops->data_received_cb) {
		audio_dev_data->ops->data_received_cb(common->dev,
						      buffer, ret_bytes);
	} else {
		net_buf_unref(buffer);
	}
}

void usb_audio_register(struct device *dev,
			const struct usb_audio_ops *ops)
{
	struct usb_audio_dev_data_t *audio_dev_data = dev->driver_data;
	const struct usb_cfg_data *cfg = dev->config->config_info;
	const struct std_if_descriptor *iface_descr =
		cfg->interface_descriptor;
	const struct cs_ac_interface_descriptor_header *header =
		(struct cs_ac_interface_descriptor_header *)
		((u8_t *)iface_descr + USB_PASSIVE_IF_DESC_SIZE);

	audio_dev_data->ops = ops;
	audio_dev_data->common.dev = dev;
	audio_dev_data->rx_enable = false;
	audio_dev_data->tx_enable = false;
	audio_dev_data->header_descr = header;

	sys_slist_append(&usb_audio_data_devlist,
			 &audio_dev_data->common.node);

	LOG_DBG("Device dev %p dev_data %p cfg %p added to devlist %p",
		dev, audio_dev_data, dev->config->config_info,
		&usb_audio_data_devlist);
}

#define DEFINE_AUDIO_DEVICE(dev, i)					  \
	USBD_CFG_DATA_DEFINE(primary, audio)				  \
	struct usb_cfg_data dev##_audio_config_##i = {			  \
		.usb_device_description	= NULL,				  \
		.interface_config = audio_interface_config,		  \
		.interface_descriptor = &dev##_desc_##i.std_ac_interface, \
		.cb_usb_status = audio_cb_usb_status,			  \
		.interface = {						  \
			.class_handler = audio_class_handle_req,	  \
			.custom_handler = audio_custom_handler,		  \
			.vendor_handler = NULL,				  \
		},							  \
		.num_endpoints = ARRAY_SIZE(dev##_usb_audio_ep_data_##i), \
		.endpoint = dev##_usb_audio_ep_data_##i,		  \
	};								  \
	DEVICE_AND_API_INIT(dev##_usb_audio_device_##i,			  \
			    DT_INST_##i##_USB_AUDIO_##dev##_LABEL,	  \
			    &usb_audio_device_init,			  \
			    &dev##_audio_dev_data_##i,			  \
			    &dev##_audio_config_##i, APPLICATION,	  \
			    CONFIG_KERNEL_INIT_PRIORITY_DEVICE,		  \
			    DUMMY_API)

#define DEFINE_BUF_POOL(name, size) \
	NET_BUF_POOL_FIXED_DEFINE(name, 5, size, net_buf_destroy)

#define UNIDIR_DEVICE(dev, i, out_pool, in_size, it_type, ot_type, cb, addr) \
	UTIL_EXPAND( \
	DEFINE_AUDIO_DEV_DATA(dev, i, out_pool, in_size); \
	DECLARE_DESCRIPTOR(dev, i, 1); \
	DEFINE_AUDIO_DESCRIPTOR(dev, i, dev##_ID(i), dev##_LINK(i), \
				it_type, ot_type, cb, addr); \
	DEFINE_AUDIO_DEVICE(dev, i))

#define HEADPHONES_DEVICE(i, dev) UTIL_EXPAND( \
	DEFINE_BUF_POOL(audio_data_pool_hp_##i, EP_SIZE(dev, i));  \
	UNIDIR_DEVICE(dev, i, &audio_data_pool_hp_##i, 0, \
		      USB_AUDIO_USB_STREAMING, USB_AUDIO_OUT_HEADPHONES, \
		      audio_receive_cb, AUTO_EP_OUT);)

#define MICROPHONE_DEVICE(i, dev) UTIL_EXPAND( \
	UNIDIR_DEVICE(dev, i, NULL, EP_SIZE(dev, i), \
		      USB_AUDIO_IN_MICROPHONE, USB_AUDIO_USB_STREAMING, \
		      usb_transfer_ep_callback, AUTO_EP_IN);)

#define HEADSET_DEVICE(i, dev) UTIL_EXPAND( \
	DEFINE_BUF_POOL(audio_data_pool_hs_##i, EP_SIZE(dev##_HP, i)); \
	DEFINE_AUDIO_DEV_DATA_BIDIR(dev, i, &audio_data_pool_hs_##i, \
				    EP_SIZE(dev##_MIC, i)); \
	DECLARE_DESCRIPTOR_BIDIR(dev, i, 2); \
	DEFINE_AUDIO_DESCRIPTOR_BIDIR(dev, i, dev##_ID(i)); \
	DEFINE_AUDIO_DEVICE(dev, i);)

UTIL_LISTIFY(HEADPHONES_DEVICE_COUNT, HEADPHONES_DEVICE, HP)
UTIL_LISTIFY(MICROPHONE_DEVICE_COUNT, MICROPHONE_DEVICE, MIC)
UTIL_LISTIFY(HEADSET_DEVICE_COUNT, HEADSET_DEVICE, HS)
