use std::os::raw::*;
// use std::ffi;
use std::ptr;

// platform-specific aliases are unknown
// IMPORTANT: these are alises to the same level of the bindings
// the values must be defined by the user

#[allow(dead_code, non_camel_case_types)]
// pub type khronos_utime_nanoseconds_t = super::khronos_utime_nanoseconds_t;
pub type khronos_utime_nanoseconds_t = u64;

#[allow(dead_code, non_camel_case_types)]
// pub type khronos_uint64_t = super::khronos_uint64_t;
pub type khronos_uint64_t = u64;

#[allow(dead_code, non_camel_case_types)]
// pub type khronos_ssize_t = super::khronos_ssize_t;
pub type khronos_ssize_t = isize;

// pub type EGLNativeDisplayType = super::EGLNativeDisplayType;
pub struct MyNativeDisplayType;
#[allow(dead_code)]
pub type EGLNativeDisplayType = *const MyNativeDisplayType;

// pub type EGLNativePixmapType = super::EGLNativePixmapType;
pub struct MyNativePixmapType;
#[allow(dead_code)]
pub type EGLNativePixmapType = *const MyNativePixmapType;

// pub type EGLNativeWindowType = super::EGLNativeWindowType;
pub struct MyNativeWindowType;
#[allow(dead_code)]
pub type EGLNativeWindowType = *const MyNativeWindowType;

#[allow(dead_code)]
// pub type EGLint = super::EGLint;
pub type EGLint = i64;

// #[allow(dead_code)]
// pub type NativeDisplayType = super::NativeDisplayType;

// #[allow(dead_code)]
// pub type NativePixmapType = super::NativePixmapType;

// #[allow(dead_code)]
// pub type NativeWindowType = super::NativeWindowType;

pub struct Display;
static THE_DISPLAY: Display = Display;

#[derive(PartialEq)]
pub struct Config {
  red_size: u8,
  green_size: u8,
  blue_size: u8,
  alpha_size: u8,
  depth_size: u8,
  stencil_size: u8,
  samples: u8,
}

static CONFIGS: &[Config] = &[
  Config{ red_size: 8, green_size: 8, blue_size: 8, alpha_size: 8, depth_size: 24, stencil_size: 8, samples: 0 },
  Config{ red_size: 8, green_size: 8, blue_size: 8, alpha_size: 8, depth_size: 24, stencil_size: 8, samples: 4 },
  Config{ red_size: 5, green_size: 6, blue_size: 5, alpha_size: 0, depth_size: 0, stencil_size: 0, samples: 0 },
];

// EGL alises
pub type EGLBoolean = c_uint;
pub type EGLenum = c_uint;
// pub type EGLAttribKHR = isize;
pub type EGLAttrib = isize;
pub type EGLConfig = *const Config;
pub type EGLContext = *const c_void;
// pub type EGLDeviceEXT = *const c_void;
pub type EGLDisplay = *const Display;
pub type EGLSurface = *const c_void;
pub type EGLClientBuffer = *const c_void;
// pub type EGLImageKHR = *const c_void;
pub type EGLImage = *const c_void;
// pub type EGLOutputLayerEXT = *const c_void;
// pub type EGLOutputPortEXT = *const c_void;
// pub type EGLSyncKHR = *const c_void;
pub type EGLSync = *const c_void;
// pub type EGLTimeKHR = khronos_utime_nanoseconds_t;
pub type EGLTime = khronos_utime_nanoseconds_t;
// pub type EGLSyncNV = *const c_void;
// pub type EGLTimeNV = khronos_utime_nanoseconds_t;
// pub type EGLuint64NV = khronos_utime_nanoseconds_t;
// pub type EGLStreamKHR = *const c_void;
// pub type EGLuint64KHR = khronos_uint64_t;
// pub type EGLNativeFileDescriptorKHR = c_int;
// pub type EGLsizeiANDROID = khronos_ssize_t;
// pub type EGLSetBlobFuncANDROID = extern "system" fn(*const c_void,
//                                                     EGLsizeiANDROID,
//                                                     *const c_void,
//                                                     EGLsizeiANDROID)
//                                                     -> ();
// pub type EGLGetBlobFuncANDROID = extern "system" fn(*const c_void,
//                                                     EGLsizeiANDROID,
//                                                     *mut c_void,
//                                                     EGLsizeiANDROID)
//                                                     -> EGLsizeiANDROID;

#[repr(C)]
#[allow(non_snake_case)]
pub struct EGLClientPixmapHI {
    pData: *const c_void,
    iWidth: EGLint,
    iHeight: EGLint,
    iStride: EGLint,
}



#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT: EGLenum = 0x3088;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT_NONPRE: EGLenum = 0x308B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_FORMAT_PRE: EGLenum = 0x308C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_MASK_SIZE: EGLenum = 0x303E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_ALPHA_SIZE: EGLenum = 0x3021;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BACK_BUFFER: EGLenum = 0x3084;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ACCESS: EGLenum = 0x3002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ALLOC: EGLenum = 0x3003;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_ATTRIBUTE: EGLenum = 0x3004;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CONFIG: EGLenum = 0x3005;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CONTEXT: EGLenum = 0x3006;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_CURRENT_SURFACE: EGLenum = 0x3007;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_DISPLAY: EGLenum = 0x3008;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_MATCH: EGLenum = 0x3009;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_NATIVE_PIXMAP: EGLenum = 0x300A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_NATIVE_WINDOW: EGLenum = 0x300B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_PARAMETER: EGLenum = 0x300C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BAD_SURFACE: EGLenum = 0x300D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BIND_TO_TEXTURE_RGB: EGLenum = 0x3039;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BIND_TO_TEXTURE_RGBA: EGLenum = 0x303A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BLUE_SIZE: EGLenum = 0x3022;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_DESTROYED: EGLenum = 0x3095;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_PRESERVED: EGLenum = 0x3094;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_BUFFER_SIZE: EGLenum = 0x3020;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CLIENT_APIS: EGLenum = 0x308D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CL_EVENT_HANDLE: EGLenum = 0x309C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE: EGLenum = 0x3087;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLORSPACE_sRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_COLOR_BUFFER_TYPE: EGLenum = 0x303F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONDITION_SATISFIED: EGLenum = 0x30F6;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFIG_CAVEAT: EGLenum = 0x3027;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFIG_ID: EGLenum = 0x3028;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONFORMANT: EGLenum = 0x3042;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_CLIENT_TYPE: EGLenum = 0x3097;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_CLIENT_VERSION: EGLenum = 0x3098;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_LOST: EGLenum = 0x300E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_MAJOR_VERSION: EGLenum = 0x3098;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_MINOR_VERSION: EGLenum = 0x30FB;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT: EGLenum = 0x00000002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT: EGLenum = 0x00000001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_DEBUG: EGLenum = 0x31B0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE: EGLenum = 0x31B1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_PROFILE_MASK: EGLenum = 0x30FD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY: EGLenum = 0x31BD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CONTEXT_OPENGL_ROBUST_ACCESS: EGLenum = 0x31B2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_CORE_NATIVE_ENGINE: EGLenum = 0x305B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DEFAULT_DISPLAY: EGLNativeDisplayType = 0 as EGLNativeDisplayType;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DEPTH_SIZE: EGLenum = 0x3025;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DISPLAY_SCALING: EGLenum = 10000;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DONT_CARE: EGLint = -1 as EGLint;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_DRAW: EGLenum = 0x3059;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_EXTENSIONS: EGLenum = 0x3055;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_FALSE: EGLBoolean = 0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_FOREVER: u64 = 0xFFFFFFFFFFFFFFFF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE: EGLenum = 0x309D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_COLORSPACE_SRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_RENDERBUFFER: EGLenum = 0x30B9;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_2D: EGLenum = 0x30B1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_3D: EGLenum = 0x30B2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X: EGLenum = 0x30B4;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: EGLenum = 0x30B6;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: EGLenum = 0x30B8;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X: EGLenum = 0x30B3;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y: EGLenum = 0x30B5;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z: EGLenum = 0x30B7;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_LEVEL: EGLenum = 0x30BC;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GL_TEXTURE_ZOFFSET: EGLenum = 0x30BD;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_GREEN_SIZE: EGLenum = 0x3023;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_HEIGHT: EGLenum = 0x3056;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_HORIZONTAL_RESOLUTION: EGLenum = 0x3090;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_IMAGE_PRESERVED: EGLenum = 0x30D2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LARGEST_PBUFFER: EGLenum = 0x3058;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LEVEL: EGLenum = 0x3029;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LOSE_CONTEXT_ON_RESET: EGLenum = 0x31BF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LUMINANCE_BUFFER: EGLenum = 0x308F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_LUMINANCE_SIZE: EGLenum = 0x303D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MATCH_NATIVE_PIXMAP: EGLenum = 0x3041;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_HEIGHT: EGLenum = 0x302A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_PIXELS: EGLenum = 0x302B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_PBUFFER_WIDTH: EGLenum = 0x302C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MAX_SWAP_INTERVAL: EGLenum = 0x303C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIN_SWAP_INTERVAL: EGLenum = 0x303B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIPMAP_LEVEL: EGLenum = 0x3083;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MIPMAP_TEXTURE: EGLenum = 0x3082;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE: EGLenum = 0x3099;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_BOX: EGLenum = 0x309B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_BOX_BIT: EGLenum = 0x0200;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_MULTISAMPLE_RESOLVE_DEFAULT: EGLenum = 0x309A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_RENDERABLE: EGLenum = 0x302D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_VISUAL_ID: EGLenum = 0x302E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NATIVE_VISUAL_TYPE: EGLenum = 0x302F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NONE: EGLenum = 0x3038;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NON_CONFORMANT_CONFIG: EGLenum = 0x3051;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NOT_INITIALIZED: EGLenum = 0x3001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_CONTEXT: EGLContext = 0 as EGLContext;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_DISPLAY: EGLDisplay = 0 as EGLDisplay;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_IMAGE: EGLImage = 0 as EGLImage;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_RESET_NOTIFICATION: EGLenum = 0x31BE;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_SURFACE: EGLSurface = 0 as EGLSurface;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_SYNC: EGLSync = 0 as EGLSync;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_NO_TEXTURE: EGLenum = 0x305C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_API: EGLenum = 0x30A2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_BIT: EGLenum = 0x0008;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES2_BIT: EGLenum = 0x0004;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES3_BIT: EGLenum = 0x00000040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES_API: EGLenum = 0x30A0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENGL_ES_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_API: EGLenum = 0x30A1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_BIT: EGLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_OPENVG_IMAGE: EGLenum = 0x3096;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PBUFFER_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PIXEL_ASPECT_RATIO: EGLenum = 0x3092;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_PIXMAP_BIT: EGLenum = 0x0002;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_READ: EGLenum = 0x305A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RED_SIZE: EGLenum = 0x3024;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RENDERABLE_TYPE: EGLenum = 0x3040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RENDER_BUFFER: EGLenum = 0x3086;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_RGB_BUFFER: EGLenum = 0x308E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SAMPLES: EGLenum = 0x3031;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SAMPLE_BUFFERS: EGLenum = 0x3032;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SIGNALED: EGLenum = 0x30F2;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SINGLE_BUFFER: EGLenum = 0x3085;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SLOW_CONFIG: EGLenum = 0x3050;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_STENCIL_SIZE: EGLenum = 0x3026;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SUCCESS: EGLenum = 0x3000;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SURFACE_TYPE: EGLenum = 0x3033;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SWAP_BEHAVIOR: EGLenum = 0x3093;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SWAP_BEHAVIOR_PRESERVED_BIT: EGLenum = 0x0400;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CL_EVENT: EGLenum = 0x30FE;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CL_EVENT_COMPLETE: EGLenum = 0x30FF;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_CONDITION: EGLenum = 0x30F8;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_FENCE: EGLenum = 0x30F9;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_FLUSH_COMMANDS_BIT: EGLenum = 0x0001;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_PRIOR_COMMANDS_COMPLETE: EGLenum = 0x30F0;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_STATUS: EGLenum = 0x30F1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_SYNC_TYPE: EGLenum = 0x30F7;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_2D: EGLenum = 0x305F;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_FORMAT: EGLenum = 0x3080;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_RGB: EGLenum = 0x305D;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_RGBA: EGLenum = 0x305E;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TEXTURE_TARGET: EGLenum = 0x3081;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TIMEOUT_EXPIRED: EGLenum = 0x30F5;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_BLUE_VALUE: EGLenum = 0x3035;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_GREEN_VALUE: EGLenum = 0x3036;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_RED_VALUE: EGLenum = 0x3037;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_RGB: EGLenum = 0x3052;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRANSPARENT_TYPE: EGLenum = 0x3034;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_TRUE: EGLBoolean = 1;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_UNKNOWN: EGLint = -1 as EGLint;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_UNSIGNALED: EGLenum = 0x30F3;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VENDOR: EGLenum = 0x3053;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VERSION: EGLenum = 0x3054;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VERTICAL_RESOLUTION: EGLenum = 0x3091;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT: EGLenum = 0x3088;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_NONPRE: EGLenum = 0x308B;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_PRE: EGLenum = 0x308C;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_ALPHA_FORMAT_PRE_BIT: EGLenum = 0x0040;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE: EGLenum = 0x3087;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_LINEAR: EGLenum = 0x308A;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_LINEAR_BIT: EGLenum = 0x0020;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_VG_COLORSPACE_sRGB: EGLenum = 0x3089;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_WIDTH: EGLenum = 0x3057;
#[allow(dead_code, non_upper_case_globals)] pub const EGL_WINDOW_BIT: EGLenum = 0x0004;


#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglBindAPI(api: EGLenum) -> EGLBoolean {
  // unimplemented!()
  EGL_TRUE
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglBindTexImage(dpy: EGLDisplay, surface: EGLSurface, buffer: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglChooseConfig(dpy: EGLDisplay, attrib_list: *const EGLint, configs: *mut EGLConfig, config_size: EGLint, num_config: *mut EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglClientWaitSync(dpy: EGLDisplay, sync: EGLSync, flags: EGLint, timeout: EGLTime) -> EGLint {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCopyBuffers(dpy: EGLDisplay, surface: EGLSurface, target: EGLNativePixmapType) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreateContext(dpy: EGLDisplay, config: EGLConfig, share_context: EGLContext, attrib_list: *const EGLint) -> EGLContext {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreateImage(dpy: EGLDisplay, ctx: EGLContext, target: EGLenum, buffer: EGLClientBuffer, attrib_list: *const EGLAttrib) -> EGLImage {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreatePbufferFromClientBuffer(dpy: EGLDisplay, buftype: EGLenum, buffer: EGLClientBuffer, config: EGLConfig, attrib_list: *const EGLint) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreatePbufferSurface(dpy: EGLDisplay, config: EGLConfig, attrib_list: *const EGLint) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreatePixmapSurface(dpy: EGLDisplay, config: EGLConfig, pixmap: EGLNativePixmapType, attrib_list: *const EGLint) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreatePlatformPixmapSurface(dpy: EGLDisplay, config: EGLConfig, native_pixmap: *mut c_void, attrib_list: *const EGLAttrib) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreatePlatformWindowSurface(dpy: EGLDisplay, config: EGLConfig, native_window: *mut c_void, attrib_list: *const EGLAttrib) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreateSync(dpy: EGLDisplay, type_: EGLenum, attrib_list: *const EGLAttrib) -> EGLSync {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglCreateWindowSurface(dpy: EGLDisplay, config: EGLConfig, win: EGLNativeWindowType, attrib_list: *const EGLint) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglDestroyContext(dpy: EGLDisplay, ctx: EGLContext) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglDestroyImage(dpy: EGLDisplay, image: EGLImage) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglDestroySurface(dpy: EGLDisplay, surface: EGLSurface) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglDestroySync(dpy: EGLDisplay, sync: EGLSync) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetConfigAttrib(dpy: EGLDisplay, config: EGLConfig, attribute: EGLint, value: *mut EGLint) -> EGLBoolean {
  let config = unsafe{ ptr::read(config) };
  let result = match attribute as EGLenum {
    EGL_CONFIG_ID => {
      CONFIGS.iter().position(|c| c == &config).unwrap() as EGLint
    },
    EGL_RENDERABLE_TYPE => {
      // TODO: GLES 1.0?
      (EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint
    },
    EGL_CONFORMANT => {
      // TODO: GLES 1.0?
      (EGL_OPENGL_ES3_BIT | EGL_OPENGL_ES2_BIT) as EGLint
    },
    EGL_RED_SIZE => { config.red_size as EGLint },
    EGL_GREEN_SIZE => { config.green_size as EGLint },
    EGL_BLUE_SIZE => { config.blue_size as EGLint },
    EGL_ALPHA_SIZE => { config.alpha_size as EGLint },
    EGL_BUFFER_SIZE => { (config.red_size + config.green_size + config.blue_size + config.alpha_size) as EGLint },
    EGL_DEPTH_SIZE => { config.depth_size as EGLint },
    EGL_STENCIL_SIZE => { config.stencil_size as EGLint },
    EGL_SAMPLES => { config.samples as EGLint },
    EGL_LUMINANCE_SIZE => { 0 },
    EGL_ALPHA_MASK_SIZE => { 0 },
    EGL_SURFACE_TYPE => {
      EGL_PBUFFER_BIT as EGLint
    },
    // x => unimplemented!("{:x}", x),
    _ => { return EGL_FALSE; },
  };

  unsafe { ptr::write(value, result); }

  EGL_TRUE
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetConfigs(dpy: EGLDisplay, configs: *mut EGLConfig, config_size: EGLint, num_config: *mut EGLint) -> EGLBoolean {
  if configs == ptr::null_mut() {
    unsafe { ptr::write(num_config, CONFIGS.len() as EGLint); }
  } else {
    let num = (CONFIGS.len() as EGLint).min(config_size);

    // TODO: maybe have CONFIGS store &Config so this is a simple memcpy
    for i in 0..num {
      unsafe { ptr::write(configs.offset(i as _), &CONFIGS[i as usize]); }
    }
    unsafe { ptr::write(num_config, num); }
  }

  EGL_TRUE
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetCurrentContext() -> EGLContext {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetCurrentDisplay() -> EGLDisplay {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetCurrentSurface(readdraw: EGLint) -> EGLSurface {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetDisplay(display_id: EGLNativeDisplayType) -> EGLDisplay {
  &THE_DISPLAY as *const _
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetError() -> EGLint {
  EGL_SUCCESS as EGLint
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetPlatformDisplay(platform: EGLenum, native_display: *mut c_void, attrib_list: *const EGLAttrib) -> EGLDisplay {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetProcAddress(procname: *const c_char) -> Option<extern "system" fn() -> ()> {
  // let cstr = unsafe { ffi::CStr::from_ptr(procname) };
  // unimplemented!("{:?}", cstr);

  // ptr::null() as _
  None
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglGetSyncAttrib(dpy: EGLDisplay, sync: EGLSync, attribute: EGLint, value: *mut EGLAttrib) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglInitialize(dpy: EGLDisplay, major: *mut EGLint, minor: *mut EGLint) -> EGLBoolean {
  // unimplemented!()
  unsafe {
    ptr::write(major, 1);
    ptr::write(minor, 5);
  }
  EGL_TRUE
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglMakeCurrent(dpy: EGLDisplay, draw: EGLSurface, read: EGLSurface, ctx: EGLContext) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglQueryAPI() -> EGLenum {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglQueryContext(dpy: EGLDisplay, ctx: EGLContext, attribute: EGLint, value: *mut EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglQueryString(dpy: EGLDisplay, name: EGLint) -> *const c_char {
  match name as EGLenum {
    EGL_CLIENT_APIS => b"OpenGL_ES" as *const u8 as *const i8,
    EGL_EXTENSIONS => b"" as *const u8 as *const i8,
    EGL_VENDOR => b"MyGL" as *const u8 as *const i8,
    EGL_VERSION => b"1.5" as *const u8 as *const i8,
    _ => {
      // TODO: set EGL_BAD_PARAMETER error
      ptr::null()
    },
  }
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglQuerySurface(dpy: EGLDisplay, surface: EGLSurface, attribute: EGLint, value: *mut EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglReleaseTexImage(dpy: EGLDisplay, surface: EGLSurface, buffer: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglReleaseThread() -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglSurfaceAttrib(dpy: EGLDisplay, surface: EGLSurface, attribute: EGLint, value: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglSwapBuffers(dpy: EGLDisplay, surface: EGLSurface) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglSwapInterval(dpy: EGLDisplay, interval: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglTerminate(dpy: EGLDisplay) -> EGLBoolean {
  EGL_TRUE
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglWaitClient() -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglWaitGL() -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglWaitNative(engine: EGLint) -> EGLBoolean {
  unimplemented!()
}

#[allow(unused_variables, non_snake_case)]
#[no_mangle]
pub extern "C" fn eglWaitSync(dpy: EGLDisplay, sync: EGLSync, flags: EGLint) -> EGLBoolean {
  unimplemented!()
}
