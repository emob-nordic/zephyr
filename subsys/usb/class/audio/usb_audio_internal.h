/*
 * USB audio class internal header
 *
 * Copyright (c) 2020 Nordic Semiconductor ASA
 *
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @file
 * @brief USB Audio Device Class internal header
 *
 * This header file is used to store internal configuration
 * defines.
 */

#ifndef ZEPHYR_INCLUDE_USB_CLASS_AUDIO_INTERNAL_H_
#define ZEPHYR_INCLUDE_USB_CLASS_AUDIO_INTERNAL_H_

#define DUMMY_API  (const void *)1

#define USB_PASSIVE_IF_DESC_SIZE sizeof(struct usb_if_descriptor)
#define USB_AC_CS_IF_DESC_SIZE sizeof(struct as_cs_interface_descriptor)
#define USB_FORMAT_TYPE_I_DESC_SIZE sizeof(struct format_type_i_descriptor)
#define USB_STD_AS_AD_EP_DESC_SIZE sizeof(struct std_as_ad_endpoint_descriptor)
#define USB_CS_AS_AD_EP_DESC_SIZE sizeof(struct cs_as_ad_ep_descriptor)
#define USB_ACTIVE_IF_DESC_SIZE (USB_PASSIVE_IF_DESC_SIZE + \
				 USB_AC_CS_IF_DESC_SIZE + \
				 USB_FORMAT_TYPE_I_DESC_SIZE + \
				 USB_STD_AS_AD_EP_DESC_SIZE + \
				 USB_CS_AS_AD_EP_DESC_SIZE)

#define INPUT_TERMINAL_DESC_SIZE sizeof(struct input_terminal_descriptor)
#define OUTPUT_TERMINAL_DESC_SIZE sizeof(struct output_terminal_descriptor)

#define BMA_CONTROLS_OFFSET 6
#define FU_FIXED_ELEMS_SIZE 7
#define DESC_bLength 0

#define GET_RES(dev, i) \
	DT_INST_##i##_USB_AUDIO_##dev##_RESOLUTION

#define HEADPHONES_DEVICE_COUNT  DT_NUM_INST(usb_audio_hp)
#define MICROPHONE_DEVICE_COUNT  DT_NUM_INST(usb_audio_mic)
#define HEADSET_DEVICE_COUNT     DT_NUM_INST(usb_audio_hs)

#define HP_ID(i) ((3*i) + 1)
#define MIC_ID(i) ((3*(HEADPHONES_DEVICE_COUNT + i)) + 1)

#define HS_ID(i) ((3*(HEADPHONES_DEVICE_COUNT + \
		      MICROPHONE_DEVICE_COUNT)) + 6*i + 1)

#define HP_LINK(i) ((3*i) + 1)
#define MIC_LINK(i) ((3*(HEADPHONES_DEVICE_COUNT + i)) + 3)

#define SYNC_TYPE_HP(i)   3
#define SYNC_TYPE_MIC(i)  DT_INST_##i##_USB_AUDIO_MIC_SYNC_TYPE_ENUM
#define SYNC_TYPE_HS_HP(i) 3
#define SYNC_TYPE_HS_MIC(i)   DT_INST_##i##_USB_AUDIO_HS_MIC_SYNC_TYPE_ENUM
#define SYNC_TYPE(dev, i) \
	(SYNC_TYPE_##dev(i) << 2)

#define FEATURES(dev, i) (0x0000					     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_MUTE,		     \
	(BIT(0)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_VOLUME,		     \
	(BIT(1)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_TONE_CONTROL,	     \
	(BIT(2) | BIT(3) | BIT(4)), (0))				     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_GRAPHIC_EQUALIZER,     \
	(BIT(5)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_AUTOMATIC_GAIN_CONTROL,\
	(BIT(6)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_DELAY,		     \
	(BIT(7)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_BASS_BOOST,	     \
	(BIT(8)), (0))							     \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_FEATURE_LOUDNESS,		     \
	(BIT(9)), (0))							     \
)

#define CH_CFG(dev, i) (0x0000						  \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_L,   (BIT(0)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_R,   (BIT(1)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_C,   (BIT(2)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LFE, (BIT(3)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LS,  (BIT(4)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_RS,  (BIT(5)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LC,  (BIT(6)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_RC,  (BIT(7)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_S,   (BIT(8)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_SL,  (BIT(9)), (0)) \
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_SR,  (BIT(10)), (0))\
| COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_T,   (BIT(11)), (0))\
)

#define CH_CNT(dev, i) (0					    \
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_L,   (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_R,   (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_C,   (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LFE, (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LS,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_RS,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_LC,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_RC,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_S,   (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_SL,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_SR,  (1), (0))\
+ COND_CODE_1(DT_INST_##i##_USB_AUDIO_##dev##_CHANNEL_T,   (1), (0))\
)

#define EP_SIZE(dev, i) \
	((GET_RES(dev, i)/8) * CH_CNT(dev, i) * 48)

/**
 * Addressable logical object inside an audio function.
 * Entity is one of: Terminal or Unit.
 * Refer to 1.4 Terms and Abbreviations from audio10.pdf
 */
struct usb_audio_entity {
	enum usb_audio_cs_ac_int_desc_subtypes subtype;
	u8_t id;
};

/** USB Audio device controls
 *
 * This structure present the structure of possible features that USB audio
 * device may support. Enabling features is done via DT overlay.
 */
struct usb_audio_features {
	bool  mute;
	u16_t volume;
	u8_t  tone_control[3];
	/* Left for time being as u8_t
	 * check Table 5-27 audio10.pdf
	 */
	u8_t  graphic_equalizer;
	bool  automatic_gain_control;
	u16_t delay;
	bool  bass_boost;
	bool  loudness;
};

/**
 * @warning Size of baInterface is 2 just to make it useable
 * for all kind of devices: headphones, microphone and headset.
 * Actual size of the struct should be checked by reading
 * .bLength.
 */
struct cs_ac_interface_descriptor_header {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u16_t bcdADC;
	u16_t wTotalLength;
	u8_t bInCollection;
	u8_t baInterfaceNr[2];
} __packed;

struct input_terminal_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u8_t bTerminalID;
	u16_t wTerminalType;
	u8_t bAssocTerminal;
	u8_t bNrChannels;
	u16_t wChannelConfig;
	u8_t iChannelNames;
	u8_t iTerminal;
} __packed;

/**
 * @note Size of Feature unit descriptor is not fixed.
 * This structure is just a helper not a common type.
 */
struct feature_unit_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u8_t bUnitID;
	u8_t bSourceID;
	u8_t bControlSize;
	u16_t bmaControls[1];
} __packed;

struct output_terminal_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u8_t bTerminalID;
	u16_t wTerminalType;
	u8_t bAssocTerminal;
	u8_t bSourceID;
	u8_t iTerminal;
} __packed;

struct as_cs_interface_descriptor {
	u8_t  bLength;
	u8_t  bDescriptorType;
	u8_t  bDescriptorSubtype;
	u8_t  bTerminalLink;
	u8_t  bDelay;
	u16_t wFormatTag;
} __packed;

struct format_type_i_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u8_t bFormatType;
	u8_t bNrChannels;
	u8_t bSubframeSize;
	u8_t bBitResolution;
	u8_t bSamFreqType;
	u8_t tSamFreq[3];
} __packed;

struct std_as_ad_endpoint_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bEndpointAddress;
	u8_t bmAttributes;
	u16_t wMaxPacketSize;
	u8_t bInterval;
	u8_t bRefresh;
	u8_t bSynchAddress;
} __packed;

struct cs_as_ad_ep_descriptor {
	u8_t bLength;
	u8_t bDescriptorType;
	u8_t bDescriptorSubtype;
	u8_t bmAttributes;
	u8_t bLockDelayUnits;
	u16_t wLockDelay;
} __packed;

#define DECLARE_HEADER(dev, i, ifaces)			\
struct dev##_cs_ac_interface_descriptor_header_##i {	\
	u8_t bLength;					\
	u8_t bDescriptorType;				\
	u8_t bDescriptorSubtype;			\
	u16_t bcdADC;					\
	u16_t wTotalLength;				\
	u8_t bInCollection;				\
	u8_t baInterfaceNr[ifaces];			\
} __packed

#define DECLARE_FEATURE_UNIT(dev, i)		\
struct dev##_feature_unit_descriptor_##i {	\
	u8_t bLength;				\
	u8_t bDescriptorType;			\
	u8_t bDescriptorSubtype;		\
	u8_t bUnitID;				\
	u8_t bSourceID;				\
	u8_t bControlSize;			\
	u16_t bmaControls[CH_CNT(dev, i) + 1];	\
	u8_t iFeature;				\
} __packed

#ifdef CONFIG_USB_COMPOSITE_DEVICE
#define DECLARE_DESCRIPTOR(dev, i, ifaces)				\
DECLARE_HEADER(dev, i, ifaces);						\
DECLARE_FEATURE_UNIT(dev, i);						\
struct dev##_descriptor_##i {						\
	struct usb_association_descriptor iad;				\
	struct usb_if_descriptor std_ac_interface;			\
	struct dev##_cs_ac_interface_descriptor_header_##i		\
						ac_interface_header;	\
	struct input_terminal_descriptor input_terminal;		\
	struct dev##_feature_unit_descriptor_##i feature_unit;		\
	struct output_terminal_descriptor output_terminal;		\
	struct usb_if_descriptor as_interface_alt_0;			\
	struct usb_if_descriptor as_interface_alt_1;			\
		struct as_cs_interface_descriptor as_cs_interface;	\
		struct format_type_i_descriptor format;			\
		struct std_as_ad_endpoint_descriptor std_ep_desc;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc;		\
} __packed
#else
#define DECLARE_DESCRIPTOR(dev, i, ifaces)				\
DECLARE_HEADER(dev, i, ifaces);						\
DECLARE_FEATURE_UNIT(dev, i);						\
struct dev##_descriptor_##i {						\
	struct usb_if_descriptor std_ac_interface;			\
	struct dev##_cs_ac_interface_descriptor_header_##i		\
						ac_interface_header;	\
	struct input_terminal_descriptor input_terminal;		\
	struct dev##_feature_unit_descriptor_##i feature_unit;		\
	struct output_terminal_descriptor output_terminal;		\
	struct usb_if_descriptor as_interface_alt_0;			\
	struct usb_if_descriptor as_interface_alt_1;			\
		struct as_cs_interface_descriptor as_cs_interface;	\
		struct format_type_i_descriptor format;			\
		struct std_as_ad_endpoint_descriptor std_ep_desc;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc;		\
} __packed
#endif

#ifdef CONFIG_USB_COMPOSITE_DEVICE
#define DECLARE_DESCRIPTOR_BIDIR(dev, i, ifaces)			\
DECLARE_HEADER(dev, i, ifaces);						\
DECLARE_FEATURE_UNIT(dev##_MIC, i);					\
DECLARE_FEATURE_UNIT(dev##_HP, i);					\
struct dev##_descriptor_##i {						\
	struct usb_association_descriptor iad;				\
	struct usb_if_descriptor std_ac_interface;			\
	struct dev##_cs_ac_interface_descriptor_header_##i		\
						ac_interface_header;	\
	struct input_terminal_descriptor input_terminal_0;		\
	struct dev##_MIC_feature_unit_descriptor_##i feature_unit_0;	\
	struct output_terminal_descriptor output_terminal_0;		\
	struct input_terminal_descriptor input_terminal_1;		\
	struct dev##_HP_feature_unit_descriptor_##i feature_unit_1;	\
	struct output_terminal_descriptor output_terminal_1;		\
	struct usb_if_descriptor as_interface_alt_0_0;			\
	struct usb_if_descriptor as_interface_alt_0_1;			\
		struct as_cs_interface_descriptor as_cs_interface_0;	\
		struct format_type_i_descriptor format_0;		\
		struct std_as_ad_endpoint_descriptor std_ep_desc_0;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc_0;		\
	struct usb_if_descriptor as_interface_alt_1_0;			\
	struct usb_if_descriptor as_interface_alt_1_1;			\
		struct as_cs_interface_descriptor as_cs_interface_1;	\
		struct format_type_i_descriptor format_1;		\
		struct std_as_ad_endpoint_descriptor std_ep_desc_1;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc_1;		\
} __packed
#else
#define DECLARE_DESCRIPTOR_BIDIR(dev, i, ifaces)			\
DECLARE_HEADER(dev, i, ifaces);						\
DECLARE_FEATURE_UNIT(dev##_MIC, i);					\
DECLARE_FEATURE_UNIT(dev##_HP, i);					\
struct dev##_descriptor_##i {						\
	struct usb_if_descriptor std_ac_interface;			\
	struct dev##_cs_ac_interface_descriptor_header_##i		\
						ac_interface_header;	\
	struct input_terminal_descriptor input_terminal_0;		\
	struct dev##_MIC_feature_unit_descriptor_##i feature_unit_0;	\
	struct output_terminal_descriptor output_terminal_0;		\
	struct input_terminal_descriptor input_terminal_1;		\
	struct dev##_HP_feature_unit_descriptor_##i feature_unit_1;	\
	struct output_terminal_descriptor output_terminal_1;		\
	struct usb_if_descriptor as_interface_alt_0_0;			\
	struct usb_if_descriptor as_interface_alt_0_1;			\
		struct as_cs_interface_descriptor as_cs_interface_0;	\
		struct format_type_i_descriptor format_0;		\
		struct std_as_ad_endpoint_descriptor std_ep_desc_0;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc_0;		\
	struct usb_if_descriptor as_interface_alt_1_0;			\
	struct usb_if_descriptor as_interface_alt_1_1;			\
		struct as_cs_interface_descriptor as_cs_interface_1;	\
		struct format_type_i_descriptor format_1;		\
		struct std_as_ad_endpoint_descriptor std_ep_desc_1;	\
		struct cs_as_ad_ep_descriptor cs_ep_desc_1;		\
} __packed
#endif

#define INIT_IAD(iface_subclass)					\
{									\
	.bLength = sizeof(struct usb_association_descriptor),		\
	.bDescriptorType = USB_ASSOCIATION_DESC,			\
	.bFirstInterface = 0,						\
	.bInterfaceCount = 0x02,					\
	.bFunctionClass = AUDIO_CLASS,					\
	.bFunctionSubClass = iface_subclass,				\
	.bFunctionProtocol = 0,						\
	.iFunction = 0,							\
}

#ifdef CONFIG_USB_COMPOSITE_DEVICE
#define USB_AUDIO_IAD  .iad = INIT_IAD(USB_AUDIO_AUDIOCONTROL),
#else
#define USB_AUDIO_IAD
#endif

#define INIT_STD_IF(iface_subclass, iface_num, alt_setting, eps_num)	\
{									\
	.bLength = sizeof(struct usb_if_descriptor),			\
	.bDescriptorType = USB_INTERFACE_DESC,				\
	.bInterfaceNumber = iface_num,					\
	.bAlternateSetting = alt_setting,				\
	.bNumEndpoints = eps_num,					\
	.bInterfaceClass = AUDIO_CLASS,					\
	.bInterfaceSubClass = iface_subclass,				\
	.bInterfaceProtocol = 0,					\
	.iInterface = 0,						\
}

#define INIT_CS_AC_IF_HEADER(dev, i, ifaces)				      \
{									      \
	.bLength = sizeof(struct dev##_cs_ac_interface_descriptor_header_##i),\
	.bDescriptorType = USB_CS_INTERFACE_DESC,			      \
	.bDescriptorSubtype = USB_AUDIO_HEADER,				      \
	.bcdADC = sys_cpu_to_le16(0x0100),				      \
	.wTotalLength = sys_cpu_to_le16(				      \
		sizeof(struct dev##_cs_ac_interface_descriptor_header_##i) +  \
		INPUT_TERMINAL_DESC_SIZE +				      \
		sizeof(struct dev##_feature_unit_descriptor_##i) +	      \
		OUTPUT_TERMINAL_DESC_SIZE),				      \
	.bInCollection = ifaces,					      \
	.baInterfaceNr = {0},						      \
}

#define INIT_CS_AC_IF_HEADER_BIDIR(dev, i, ifaces)			      \
{									      \
	.bLength = sizeof(struct dev##_cs_ac_interface_descriptor_header_##i),\
	.bDescriptorType = USB_CS_INTERFACE_DESC,			      \
	.bDescriptorSubtype = USB_AUDIO_HEADER,				      \
	.bcdADC = sys_cpu_to_le16(0x0100),				      \
	.wTotalLength = sys_cpu_to_le16(				      \
		sizeof(struct dev##_cs_ac_interface_descriptor_header_##i) +  \
		2*INPUT_TERMINAL_DESC_SIZE +				      \
		sizeof(struct dev##_MIC_feature_unit_descriptor_##i) +	      \
		sizeof(struct dev##_HP_feature_unit_descriptor_##i) +	      \
		2*OUTPUT_TERMINAL_DESC_SIZE),				      \
	.bInCollection = ifaces,					      \
	.baInterfaceNr = {0},						      \
}

#define INIT_IN_TERMINAL(dev, i, terminal_id, type)		\
{								\
	.bLength = INPUT_TERMINAL_DESC_SIZE,			\
	.bDescriptorType = USB_CS_INTERFACE_DESC,		\
	.bDescriptorSubtype = USB_AUDIO_INPUT_TERMINAL,		\
	.bTerminalID = terminal_id,				\
	.wTerminalType = sys_cpu_to_le16(type),			\
	.bAssocTerminal = 0,					\
	.bNrChannels = MAX(1, CH_CNT(dev, i)),			\
	.wChannelConfig = sys_cpu_to_le16(CH_CFG(dev, i)),	\
	.iChannelNames = 0,					\
	.iTerminal = 0,						\
}

#define INIT_OUT_TERMINAL(terminal_id, source_id, type)		\
{								\
	.bLength = OUTPUT_TERMINAL_DESC_SIZE,			\
	.bDescriptorType = USB_CS_INTERFACE_DESC,		\
	.bDescriptorSubtype = USB_AUDIO_OUTPUT_TERMINAL,	\
	.bTerminalID = terminal_id,				\
	.wTerminalType = sys_cpu_to_le16(type),			\
	.bAssocTerminal = 0,					\
	.bSourceID = source_id,					\
	.iTerminal = 0,						\
}

/** refer to Table 4-7 from audio10.pdf
 */
#define INIT_FEATURE_UNIT(dev, i, unit_id, source_id)			\
{									\
	.bLength = sizeof(struct dev##_feature_unit_descriptor_##i),	\
	.bDescriptorType = USB_CS_INTERFACE_DESC,			\
	.bDescriptorSubtype = USB_AUDIO_FEATURE_UNIT,			\
	.bUnitID = unit_id,						\
	.bSourceID = source_id,						\
	.bControlSize = sizeof(u16_t),					\
	.bmaControls = { FEATURES(dev, i) },				\
	.iFeature = 0,							\
}

/* Class-Specific AS Interface Descriptor 4.5.2 audio10.pdf */
#define INIT_AS_GENERAL(link)				\
{							\
	.bLength = USB_AC_CS_IF_DESC_SIZE,		\
	.bDescriptorType = USB_CS_INTERFACE_DESC,	\
	.bDescriptorSubtype = USB_AUDIO_AS_GENERAL,	\
	.bTerminalLink = link,				\
	.bDelay = 0,					\
	.wFormatTag = sys_cpu_to_le16(0x0001),		\
}

/** Class-Specific AS Format Type Descriptor 4.5.3 audio10.pdf
 *  For more information refer to 2.2.5 Type I Format Type Descriptor
 *  from frmts10.pdf
 */
#define INIT_AS_FORMAT_I(ch_cnt, res)				\
{								\
	.bLength = sizeof(struct format_type_i_descriptor),	\
	.bDescriptorType = USB_CS_INTERFACE_DESC,		\
	.bDescriptorSubtype = USB_AUDIO_FORMAT_TYPE,		\
	.bFormatType = 0x01,					\
	.bNrChannels = MAX(1, ch_cnt),				\
	.bSubframeSize = res/8,					\
	.bBitResolution = res,					\
	.bSamFreqType = 1,					\
	.tSamFreq = {0x80, 0xBB, 0x00},				\
}

#define INIT_STD_AS_AD_EP(dev, i, addr)					\
{									\
	.bLength = sizeof(struct std_as_ad_endpoint_descriptor),	\
	.bDescriptorType = USB_ENDPOINT_DESC,				\
	.bEndpointAddress = addr,					\
	.bmAttributes = (USB_DC_EP_ISOCHRONOUS | SYNC_TYPE(dev, i)),	\
	.wMaxPacketSize = sys_cpu_to_le16(EP_SIZE(dev, i)),		\
	.bInterval = 0x01,						\
	.bRefresh = 0x00,						\
	.bSynchAddress = 0x00,						\
}

#define INIT_CS_AS_AD_EP					\
{								\
	.bLength = sizeof(struct cs_as_ad_ep_descriptor),	\
	.bDescriptorType = USB_CS_ENDPOINT_DESC,		\
	.bDescriptorSubtype = 0x01,				\
	.bmAttributes = 0x00,					\
	.bLockDelayUnits = 0x00,				\
	.wLockDelay = 0,					\
}

#define INIT_EP_DATA(cb, addr)	\
	{			\
		.ep_cb = cb,	\
		.ep_addr = addr,\
	}

#endif /* ZEPHYR_INCLUDE_USB_CLASS_AUDIO_INTERNAL_H_ */
