#version 440 core
in vec3 vertex;
in vec2 texture_coords;
out vec2 v_texcoords;
uniform mat4 model;
uniform mat4 view;

void main() {
    v_texcoords = texture_coords;
    gl_Position = view * model * vec4(vertex, 1.0);
}
