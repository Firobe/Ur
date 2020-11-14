#version 440 core
in vec3 vertex;
in vec3 color;
out vec4 v_color;
uniform mat4 model;
uniform mat4 view;

void main() {
    v_color = vec4(color, 1.0);
    gl_Position = view * model * vec4(vertex, 1.0);
}
