#version 440 core
in vec2 v_texcoords;
uniform sampler2D tex0;
out vec4 color;

void main() {
    color = texture2D(tex0, v_texcoords);
}
