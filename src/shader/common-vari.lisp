(in-package #:virality.shaders)

(v-def-glsl-template-fun vec2 (a) "vec2(~a)" (:vec2) :vec2 :pure t)
(v-def-glsl-template-fun vec2 (a) "vec2(~a)" (:vec3) :vec2 :pure t)
(v-def-glsl-template-fun vec2 (a) "vec2(~a)" (:vec4) :vec2 :pure t)
(v-def-glsl-template-fun vec3 (a) "vec3(~a)" (:vec3) :vec3 :pure t)
(v-def-glsl-template-fun vec3 (a) "vec3(~a)" (:vec4) :vec3 :pure t)
(v-def-glsl-template-fun vec4 (a) "vec4(~a)" (:vec4) :vec4 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:float) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat2) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat2x2) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat2x3) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat2x4) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat3x2) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat3) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat3x3) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat3x4) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat4x2) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat4x3) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat4) :mat2 :pure t)
(v-def-glsl-template-fun mat2 (a) "mat2(~a)" (:mat4x4) :mat2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:float) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a b c d) "mat2x2(~a,~a,~a,~a)"
                         (:float :float :float :float)
                         :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a b) "mat2x2(~a,~a)" (:vec2 :vec2) :mat2x2
                         :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat2) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat2x2) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat2x3) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat2x4) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat3x2) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat3) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat3x3) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat3x4) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat4x2) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat4x3) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat4) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x2 (a) "mat2x2(~a)" (:mat4x4) :mat2x2 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:float) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a b c d e f) "mat2x3(~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float) :mat2x3
                         :pure t)
(v-def-glsl-template-fun mat2x3 (a b) "mat2x3(~a,~a)" (:vec3 :vec3) :mat2x3
                         :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat2) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat2x2) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat2x3) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat2x4) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat3x2) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat3) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat3x3) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat3x4) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat4x2) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat4x3) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat4) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x3 (a) "mat2x3(~a)" (:mat4x4) :mat2x3 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:float) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a b c d e f g h)
                         "mat2x4(~a,~a,~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float :float
                                 :float)
                         :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a b) "mat2x4(~a,~a)" (:vec4 :vec4) :mat2x4
                         :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat2) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat2x2) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat2x3) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat2x4) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat3x2) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat3) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat3x3) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat3x4) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat4x2) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat4x3) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat4) :mat2x4 :pure t)
(v-def-glsl-template-fun mat2x4 (a) "mat2x4(~a)" (:mat4x4) :mat2x4 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:float) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a b c d e f) "mat3x2(~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float) :mat3x2
                         :pure t)
(v-def-glsl-template-fun mat3x2 (a b c) "mat3x2(~a,~a,~a)" (:vec2 :vec2 :vec2)
                         :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat2) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat2x2) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat2x3) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat2x4) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat3x2) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat3) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat3x3) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat3x4) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat4x2) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat4x3) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat4) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3x2 (a) "mat3x2(~a)" (:mat4x4) :mat3x2 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:float) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat2) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat2x2) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat2x3) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat2x4) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat3x2) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat3) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat3x3) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat3x4) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat4x2) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat4x3) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat4) :mat3 :pure t)
(v-def-glsl-template-fun mat3 (a) "mat3(~a)" (:mat4x4) :mat3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:float) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a b c d e f g h i)
                         "mat3x3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float :float
                                 :float :float)
                         :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a b c) "mat3x3(~a,~a,~a)" (:vec3 :vec3 :vec3)
                         :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat2) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat2x2) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat2x3) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat2x4) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat3x2) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat3) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat3x3) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat3x4) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat4x2) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat4x3) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat4) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x3 (a) "mat3x3(~a)" (:mat4x4) :mat3x3 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:float) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a b c d e f g h i j k l)
                         "mat3x4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float :float
                                 :float :float :float :float :float)
                         :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a b c) "mat3x4(~a,~a,~a)" (:vec4 :vec4 :vec4)
                         :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat2) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat2x2) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat2x3) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat2x4) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat3x2) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat3) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat3x3) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat3x4) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat4x2) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat4x3) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat4) :mat3x4 :pure t)
(v-def-glsl-template-fun mat3x4 (a) "mat3x4(~a)" (:mat4x4) :mat3x4 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:float) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a b c d e f g h)
                         "mat4x2(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float :float
                                 :float)
                         :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a b c d) "mat4x2(~a,~a,~a,~a)"
                         (:vec2 :vec2 :vec2 :vec2)
                         :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat2) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat2x2) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat2x3) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat2x4) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat3x2) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat3) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat3x3) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat3x4) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat4x2) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat4x3) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat4) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x2 (a) "mat4x2(~a)" (:mat4x4) :mat4x2 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:float) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a b c d e f g h i j k l)
                         "mat4x3(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (:float :float :float :float :float :float :float
                                 :float :float :float :float :float)
                         :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a b c d) "mat4x3(~a,~a,~a,~a)"
                         (:vec3 :vec3 :vec3 :vec3)
                         :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat2) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat2x2) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat2x3) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat2x4) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat3x2) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat3) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat3x3) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat3x4) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat4x2) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat4x3) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat4) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4x3 (a) "mat4x3(~a)" (:mat4x4) :mat4x3 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:float) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat2) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat2x2) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat2x3) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat2x4) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat3x2) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat3) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat3x3) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat3x4) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat4x2) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat4x3) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat4) :mat4 :pure t)
(v-def-glsl-template-fun mat4 (a) "mat4(~a)" (:mat4x4) :mat4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:float) :mat4x4 :pure t)
(v-def-glsl-template-fun
 mat4x4 (a b c d e f g h i j k l m n o p)
 "mat4x4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
 (:float :float :float :float :float :float :float :float :float :float :float
         :float :float :float :float :float)
 :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a b c d) "mat4x4(~a,~a,~a,~a)"
                         (:vec4 :vec4 :vec4 :vec4)
                         :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat2) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat2x2) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat2x3) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat2x4) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat3x2) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat3) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat3x3) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat3x4) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat4x2) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat4x3) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat4) :mat4x4 :pure t)
(v-def-glsl-template-fun mat4x4 (a) "mat4x4(~a)" (:mat4x4) :mat4x4 :pure t)
