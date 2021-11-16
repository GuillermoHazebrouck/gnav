--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Gl.Utility
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 03 Dec 2019
--
-- LAST_MODIFICATION : 03 Dec 2019 Copied from GtkAda project
--------------------------------------------------------------------------------
with Interfaces.C.Extensions;




--******************************************************************************
--  This file provides a very basic binding to the openGL library (they
--  were generated from the Mesa files). These are mainly provided as
--  examples, not as a full binding and are provided without support.
--******************************************************************************
package Gl.Utility is

   pragma Warnings (Off);

   GLU_VERSION_1_1 : constant := 1;
   GLU_TRUE        : constant := 1;
   GLU_FALSE       : constant := 0;

   type GLUenum is new Integer;
   for  GLUenum 'Size use 32;

   GLU_SMOOTH                  : constant GLUenum := 100000;
   GLU_FLAT                    : constant GLUenum := 100001;
   GLU_NONE                    : constant GLUenum := 100002;
   GLU_POINT                   : constant GLUenum := 100010;
   GLU_LINE                    : constant GLUenum := 100011;
   GLU_FILL                    : constant GLUenum := 100012;
   GLU_SILHOUETTE              : constant GLUenum := 100013;
   GLU_OUTSIDE                 : constant GLUenum := 100020;
   GLU_INSIDE                  : constant GLUenum := 100021;
   GLU_BEGIN                   : constant GLUenum := 100100;
   GLU_VERTEX                  : constant GLUenum := 100101;
   GLU_END                     : constant GLUenum := 100102;
   GLU_ERROR                   : constant GLUenum := 100103;
   GLU_EDGE_FLAG               : constant GLUenum := 100104;
   GLU_CW                      : constant GLUenum := 100120;
   GLU_CCW                     : constant GLUenum := 100121;
   GLU_INTERIOR                : constant GLUenum := 100122;
   GLU_EXTERIOR                : constant GLUenum := 100123;
   GLU_UNKNOWN                 : constant GLUenum := 100124;
   GLU_TESS_ERROR1             : constant GLUenum := 100151;
   GLU_TESS_ERROR2             : constant GLUenum := 100152;
   GLU_TESS_ERROR3             : constant GLUenum := 100153;
   GLU_TESS_ERROR4             : constant GLUenum := 100154;
   GLU_TESS_ERROR5             : constant GLUenum := 100155;
   GLU_TESS_ERROR6             : constant GLUenum := 100156;
   GLU_TESS_ERROR7             : constant GLUenum := 100157;
   GLU_TESS_ERROR8             : constant GLUenum := 100158;
   GLU_TESS_ERROR9             : constant GLUenum := 100159;
   GLU_AUTO_LOAD_MATRIX        : constant GLUenum := 100200;
   GLU_CULLING                 : constant GLUenum := 100201;
   GLU_PARAMETRIC_TOLERANCE    : constant GLUenum := 100202;
   GLU_SAMPLING_TOLERANCE      : constant GLUenum := 100203;
   GLU_DISPLAY_MODE            : constant GLUenum := 100204;
   GLU_SAMPLING_METHOD         : constant GLUenum := 100205;
   GLU_U_STEP                  : constant GLUenum := 100206;
   GLU_V_STEP                  : constant GLUenum := 100207;
   GLU_PATH_LENGTH             : constant GLUenum := 100215;
   GLU_PARAMETRIC_ERROR        : constant GLUenum := 100216;
   GLU_DOMAIN_DISTANCE         : constant GLUenum := 100217;
   GLU_MAP1_TRIM_2             : constant GLUenum := 100210;
   GLU_MAP1_TRIM_3             : constant GLUenum := 100211;
   GLU_OUTLINE_POLYGON         : constant GLUenum := 100240;
   GLU_OUTLINE_PATCH           : constant GLUenum := 100241;
   GLU_NURBS_ERROR1            : constant GLUenum := 100251;
   GLU_NURBS_ERROR2            : constant GLUenum := 100252;
   GLU_NURBS_ERROR3            : constant GLUenum := 100253;
   GLU_NURBS_ERROR4            : constant GLUenum := 100254;
   GLU_NURBS_ERROR5            : constant GLUenum := 100255;
   GLU_NURBS_ERROR6            : constant GLUenum := 100256;
   GLU_NURBS_ERROR7            : constant GLUenum := 100257;
   GLU_NURBS_ERROR8            : constant GLUenum := 100258;
   GLU_NURBS_ERROR9            : constant GLUenum := 100259;
   GLU_NURBS_ERROR10           : constant GLUenum := 100260;
   GLU_NURBS_ERROR11           : constant GLUenum := 100261;
   GLU_NURBS_ERROR12           : constant GLUenum := 100262;
   GLU_NURBS_ERROR13           : constant GLUenum := 100263;
   GLU_NURBS_ERROR14           : constant GLUenum := 100264;
   GLU_NURBS_ERROR15           : constant GLUenum := 100265;
   GLU_NURBS_ERROR16           : constant GLUenum := 100266;
   GLU_NURBS_ERROR17           : constant GLUenum := 100267;
   GLU_NURBS_ERROR18           : constant GLUenum := 100268;
   GLU_NURBS_ERROR19           : constant GLUenum := 100269;
   GLU_NURBS_ERROR20           : constant GLUenum := 100270;
   GLU_NURBS_ERROR21           : constant GLUenum := 100271;
   GLU_NURBS_ERROR22           : constant GLUenum := 100272;
   GLU_NURBS_ERROR23           : constant GLUenum := 100273;
   GLU_NURBS_ERROR24           : constant GLUenum := 100274;
   GLU_NURBS_ERROR25           : constant GLUenum := 100275;
   GLU_NURBS_ERROR26           : constant GLUenum := 100276;
   GLU_NURBS_ERROR27           : constant GLUenum := 100277;
   GLU_NURBS_ERROR28           : constant GLUenum := 100278;
   GLU_NURBS_ERROR29           : constant GLUenum := 100279;
   GLU_NURBS_ERROR30           : constant GLUenum := 100280;
   GLU_NURBS_ERROR31           : constant GLUenum := 100281;
   GLU_NURBS_ERROR32           : constant GLUenum := 100282;
   GLU_NURBS_ERROR33           : constant GLUenum := 100283;
   GLU_NURBS_ERROR34           : constant GLUenum := 100284;
   GLU_NURBS_ERROR35           : constant GLUenum := 100285;
   GLU_NURBS_ERROR36           : constant GLUenum := 100286;
   GLU_NURBS_ERROR37           : constant GLUenum := 100287;
   GLU_INVALID_ENUM            : constant GLUenum := 100900;
   GLU_INVALID_VALUE           : constant GLUenum := 100901;
   GLU_OUT_OF_MEMORY           : constant GLUenum := 100902;
   GLU_INCOMPATIBLE_GL_VERSION : constant GLUenum := 100903;
   GLU_VERSION                 : constant GLUenum := 100800;
   GLU_EXTENSIONS              : constant GLUenum := 100801;

   --   Normal vectors
   --   Quadric draw styles
   --   Quadric orientation
   --   Tesselator
   --   Contour types
   --   Tesselation errors
   --   NURBS
   --   Errors
   --   New in GLU 1.1
   
   -----------------------------------------------------------------------------
   --  These are the GLU 1.1 typedefs.  GLU 1.2 has different ones!
   -----------------------------------------------------------------------------

   type GLUquadricObj is new Interfaces.C.Extensions.opaque_structure_def;
   
   type GLUquadricObj_Ptr is access GLUquadricObj;
   
   type GLUtriangulatorObj is new Interfaces.C.Extensions.opaque_structure_def;
   
   type GLUtriangulatorObj_Ptr is access GLUtriangulatorObj;
   
   type GLUnurbsObj is new Interfaces.C.Extensions.opaque_structure_def;
   
   type GLUnurbsObj_Ptr is access GLUnurbsObj;

   type glu_h_proc_1 is access procedure;

   type glu_h_proc_2 is access procedure;

   type glu_h_proc_3 is access procedure;

   pragma Convention (C, glu_h_proc_1);
   pragma Convention (C, glu_h_proc_2);
   pragma Convention (C, glu_h_proc_3);

   -----------------------------------------------------------------------------
   --  Miscellaneous functions
   -----------------------------------------------------------------------------

   procedure gluLookAt (eyex    : Gl_Double;
                        eyey    : Gl_Double;
                        eyez    : Gl_Double;
                        centerx : Gl_Double;
                        centery : Gl_Double;
                        centerz : Gl_Double;
                        upx     : Gl_Double;
                        upy     : Gl_Double;
                        upz     : Gl_Double);
   
   procedure gluOrtho2D (left   : Gl_Double;
                         right  : Gl_Double;
                         bottom : Gl_Double;
                         top    : Gl_Double);
   
   procedure gluPerspective (fovy   : Gl_Double;
                             aspect : Gl_Double;
                             zNear  : Gl_Double;
                             zFar   : Gl_Double);
   
   procedure gluPickMatrix (x        : Gl_Double;
                            y        : Gl_Double;
                            width    : Gl_Double;
                            height   : Gl_Double;
                            viewport : Gl_Int_Vec_4);
   
   function gluProject (objx        : Gl_Double;
                        objy        : Gl_Double;
                        objz        : Gl_Double;
                        modelMatrix : Gl_Double_Vec_16;
                        projMatrix  : Gl_Double_Vec_16;
                        viewport    : Gl_Int_Vec_4;
                        winx        : access GL_Double;
                        winy        : access GL_Double;
                        winz        : access GL_Double) return GL_Int;
   
   function gluUnProject
       (winx        : Gl_Double;
        winy        : Gl_Double;
        winz        : Gl_Double;
        modelMatrix : Gl_Double_Vec_16;
        projMatrix  : Gl_Double_Vec_16;
        viewport    : Gl_Int_Vec_4;
        objx        : access Gl_Double;
        objy        : access Gl_Double;
        objz        : access Gl_Double) return Gl_Int;
   
   function gluErrorString (ErrorCode : Gl_Enum) return Gl_Ubyte_Ptr;

   -----------------------------------------------------------------------------
   --  Mipmapping and image scaling
   -----------------------------------------------------------------------------

   function gluScaleImage
       (format    : Gl_Enum;
        widthin   : Gl_Int;
        heightin  : Gl_Int;
        typein    : Gl_Enum;
        datain    : Interfaces.C.Extensions.Void_Ptr;
        widthout  : Gl_Int;
        heightout : Gl_Int;
        typeout   : Gl_Enum;
        dataout   : Interfaces.C.Extensions.Void_Ptr) return Gl_Int;
   
   function gluBuild1DMipmaps
       (target     : Gl_Enum;
        components : Gl_Int;
        width      : Gl_Int;
        format     : Gl_Enum;
        type_Id    : Gl_Enum;
        data       : Interfaces.C.Extensions.Void_Ptr) return Gl_Int;
   
   function gluBuild2DMipmaps
       (target     : Gl_Enum;
        components : Gl_Int;
        width      : Gl_Int;
        height     : Gl_Int;
        format     : Gl_Enum;
        type_Id    : Gl_Enum;
        data       : Interfaces.C.Extensions.Void_Ptr) return Gl_Int;

   -----------------------------------------------------------------------------
   --  Quadrics
   -----------------------------------------------------------------------------

   function gluNewQuadric return GLUquadricObj_Ptr;
   
   procedure gluDeleteQuadric (state : access GLUquadricObj);
   
   procedure gluQuadricDrawStyle (quadObject : access GLUquadricObj;
                                  drawStyle  : Gl_Enum);
   
   procedure gluQuadricOrientation (quadObject  : access GLUquadricObj;
                                    orientation : Gl_Enum);
   
   procedure gluQuadricNormals (quadObject : access GLUquadricObj;
                                normals    : Gl_Enum);
   
   procedure gluQuadricTexture (quadObject    : access GLUquadricObj;
                                textureCoords : Gl_Boolean);

   procedure gluQuadricCallback (qobj  : access GLUquadricObj;
                                 which : Gl_Enum;
                                 fn    : glu_h_proc_1);
   
   procedure gluCylinder (qobj       : access GLUquadricObj;
                          baseRadius : Gl_Double;
                          topRadius  : Gl_Double;
                          height     : Gl_Double;
                          slices     : Gl_Int;
                          stacks     : Gl_Int);
   
   procedure gluSphere (qobj   : access GLUquadricObj;
                        radius : Gl_Double;
                        slices : Gl_Int;
                        stacks : Gl_Int);
   
   procedure gluDisk (qobj        : access GLUquadricObj;
                      innerRadius : Gl_Double;
                      outerRadius : Gl_Double;
                      slices      : Gl_Int;
                      loops       : Gl_Int);
   
   procedure gluPartialDisk (qobj        : access GLUquadricObj;
                             innerRadius : Gl_Double;
                             outerRadius : Gl_Double;
                             slices      : Gl_Int;
                             loops       : Gl_Int;
                             startAngle  : Gl_Double;
                             sweepAngle  : Gl_Double);

   -----------------------------------------------------------------------------
   --  Nurbs
   -----------------------------------------------------------------------------

   function gluNewNurbsRenderer return GLUnurbsObj_Ptr;
   
   procedure gluDeleteNurbsRenderer (nobj : access GLUnurbsObj);
   
   procedure gluLoadSamplingMatrices (nobj        : access GLUnurbsObj;
                                      modelMatrix : Gl_Float_Vec_16;
                                      projMatrix  : Gl_Float_Vec_16;
                                      viewport    : Gl_Int_Vec_4);
   
   procedure gluNurbsProperty (nobj     : access GLUnurbsObj;
                               property : Gl_Enum;
                               value    : Gl_Float);
   
   procedure gluGetNurbsProperty (nobj     : access GLUnurbsObj;
                                  property : Gl_Enum;
                                  value    : access Gl_Float);
   
   procedure gluBeginCurve (nobj : access GLUnurbsObj);
   
   procedure gluEndCurve (nobj : access GLUnurbsObj);
   
   procedure gluNurbsCurve (nobj     : access GLUnurbsObj;
                            nknots   : Gl_Int;
                            knot     : access Gl_Float;
                            stride   : Gl_Int;
                            ctlarray : access Gl_Float;
                            order    : Gl_Int;
                            type_Id  : Gl_Enum);
   
   procedure gluBeginSurface (nobj : access GLUnurbsObj);
   
   procedure gluEndSurface (nobj : access GLUnurbsObj);
   
   procedure gluNurbsSurface (nobj        : access GLUnurbsObj;
                              sknot_count : Gl_Int;
                              sknot       : access Gl_Float;
                              tknot_count : Gl_Int;
                              tknot       : access Gl_Float;
                              s_stride    : Gl_Int;
                              t_stride    : Gl_Int;
                              ctlarray    : access Gl_Float;
                              sorder      : Gl_Int;
                              torder      : Gl_Int;
                              type_Id     : Gl_Enum);
   
   procedure gluBeginTrim (nobj : access GLUnurbsObj);
   
   procedure gluEndTrim (nobj : access GLUnurbsObj);
   
   procedure gluPwlCurve (nobj     : access GLUnurbsObj;
                          count    : Gl_Int;
                          array_Id : access Gl_Float;
                          stride   : Gl_Int;
                          type_Id  : Gl_Enum);

   procedure gluNurbsCallback (nobj  : access GLUnurbsObj;
                               which : Gl_Enum;
                               fn    : glu_h_proc_2);

   -----------------------------------------------------------------------------
   --  Polygon tesselation
   -----------------------------------------------------------------------------

   function gluNewTess return GLUtriangulatorObj_Ptr;

   procedure gluTessCallback (tobj  : access GLUtriangulatorObj;
                              which : Gl_Enum;
                              fn    : glu_h_proc_3);
   
   procedure gluDeleteTess (tobj : access GLUtriangulatorObj);
   
   procedure gluBeginPolygon (tobj : access GLUtriangulatorObj);
   
   procedure gluEndPolygon (tobj : access GLUtriangulatorObj);
   
   procedure gluNextContour (tobj    : access GLUtriangulatorObj;
                             type_Id : Gl_Enum);
   
   procedure gluTessVertex (tobj : access GLUtriangulatorObj;
                            v    : Gl_Double_Vec_3;
                            data : Interfaces.C.Extensions.void_ptr);

   -----------------------------------------------------------------------------
   --  New functions in GLU 1.1
   -----------------------------------------------------------------------------

   function gluGetString (name : Gl_Enum) return Gl_Ubyte_Ptr;

private

   pragma Import (Stdcall, gluLookAt, "gluLookAt");
   pragma Import (Stdcall, gluOrtho2D, "gluOrtho2D");
   pragma Import (Stdcall, gluPerspective, "gluPerspective");
   pragma Import (Stdcall, gluPickMatrix, "gluPickMatrix");
   pragma Import (Stdcall, gluProject, "gluProject");
   pragma Import (Stdcall, gluUnProject, "gluUnProject");
   pragma Import (Stdcall, gluErrorString, "gluErrorString");
   pragma Import (Stdcall, gluScaleImage, "gluScaleImage");
   pragma Import (Stdcall, gluBuild1DMipmaps, "gluBuild1DMipmaps");
   pragma Import (Stdcall, gluBuild2DMipmaps, "gluBuild2DMipmaps");
   pragma Import (Stdcall, gluNewQuadric, "gluNewQuadric");
   pragma Import (Stdcall, gluDeleteQuadric, "gluDeleteQuadric");
   pragma Import (Stdcall, gluQuadricDrawStyle, "gluQuadricDrawStyle");
   pragma Import (Stdcall, gluQuadricOrientation, "gluQuadricOrientation");
   pragma Import (Stdcall, gluQuadricNormals, "gluQuadricNormals");
   pragma Import (Stdcall, gluQuadricTexture, "gluQuadricTexture");
   pragma Import (Stdcall, gluQuadricCallback, "gluQuadricCallback");
   pragma Import (Stdcall, gluCylinder, "gluCylinder");
   pragma Import (Stdcall, gluSphere, "gluSphere");
   pragma Import (Stdcall, gluDisk, "gluDisk");
   pragma Import (Stdcall, gluPartialDisk, "gluPartialDisk");
   pragma Import (Stdcall, gluNewNurbsRenderer, "gluNewNurbsRenderer");
   pragma Import (Stdcall, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");
   pragma Import (Stdcall, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");
   pragma Import (Stdcall, gluNurbsProperty, "gluNurbsProperty");
   pragma Import (Stdcall, gluGetNurbsProperty, "gluGetNurbsProperty");
   pragma Import (Stdcall, gluBeginCurve, "gluBeginCurve");
   pragma Import (Stdcall, gluEndCurve, "gluEndCurve");
   pragma Import (Stdcall, gluNurbsCurve, "gluNurbsCurve");
   pragma Import (Stdcall, gluBeginSurface, "gluBeginSurface");
   pragma Import (Stdcall, gluEndSurface, "gluEndSurface");
   pragma Import (Stdcall, gluNurbsSurface, "gluNurbsSurface");
   pragma Import (Stdcall, gluBeginTrim, "gluBeginTrim");
   pragma Import (Stdcall, gluEndTrim, "gluEndTrim");
   pragma Import (Stdcall, gluPwlCurve, "gluPwlCurve");
   pragma Import (Stdcall, gluNurbsCallback, "gluNurbsCallback");
   pragma Import (Stdcall, gluNewTess, "gluNewTess");
   pragma Import (Stdcall, gluTessCallback, "gluTessCallback");
   pragma Import (Stdcall, gluDeleteTess, "gluDeleteTess");
   pragma Import (Stdcall, gluBeginPolygon, "gluBeginPolygon");
   pragma Import (Stdcall, gluEndPolygon, "gluEndPolygon");
   pragma Import (Stdcall, gluNextContour, "gluNextContour");
   pragma Import (Stdcall, gluTessVertex, "gluTessVertex");
   pragma Import (Stdcall, gluGetString, "gluGetString");

end Gl.Utility;
--------------------------------------------------------------------------------

