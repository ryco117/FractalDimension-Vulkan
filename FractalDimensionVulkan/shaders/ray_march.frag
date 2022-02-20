#version 450
layout (location = 0) in vec2 coord;

layout (location = 0) out vec4 fragColor;

layout (push_constant) uniform Push {
	vec3 cameraPosition;
	float time;
	vec4 cameraQuaternion;
	float aspectRatio;
} push;

const float pi = 3.14159265358;
const float tau = 2.0*pi;
const float e = 2.718281828;
const int maxIterations = 150;
const float epsilon = 0.00001;
const vec3 dirX = vec3(1.0, 0.0, 0.0);
const vec3 dirY = vec3(0.0, 1.0, 0.0);
const vec3 dirZ = vec3(0.0, 0.0, 1.0);

mat3 buildRot3(vec3 u, float theta)
{
	float c = cos(theta);
	float cC = 1.0 - c;
	float s = sin(theta);
	float sC = 1.0 - s;
	return mat3(
		c+u.x*u.x*cC, u.y*u.x*cC+u.z*s, u.z*u.x*cC-u.y*s,
		u.x*u.y*cC-u.z*s, c+u.y*u.y*cC, u.z*u.y*cC+u.x*s,
		u.x*u.z*cC+u.y*s, u.y*u.z*cC-u.x*s, c+u.z*u.z*cC
	);
}

vec3 rotateByQuaternion(vec3 v, vec4 q)
{
	vec3 temp = cross(q.xyz, cross(q.xyz, v) + q.w * v);
	return v + temp+temp;
}

float bound(float x, float b) {
	return mod(x + b, 2.0*b) - b;
}
float boundReflect(float x, float b) {
	float r = mod(x + b, 4.0*b);
	if (r < 2.0*b) {
		return r - b;
	} else {
		return 3.0*b - r;
	}
}

vec3 gradient;
vec4 orbitTrap;
float distanceEstimator(vec3 t, const int iterations)
{
	orbitTrap = vec4(1.0, 1.0, 1.0, 1.0);

	// Mandelbulb
	const int maxIterations = 4;
	const float reScale = 1.5;
	t = vec3(boundReflect(t.x, 9.0), boundReflect(t.y, 9.0), boundReflect(t.z, 9.0));
	t *= reScale;
	vec3 s = t;
	float power = 7.0 + 3.0*cos(push.time / 12.0);
	float dr = 1.0;
	float r = 0.0;
	for (int i = 0; i < maxIterations; i++) {
		r = length(s);
		const float b = 1.5;
		if (r > b) break;

		float theta = acos(s.z/r);
		float phi = atan(s.y, s.x);
		dr = pow(r, power-1.0)*power*dr + 1.0;

		r = pow(r, power);
		theta *= power;
		phi *= power;

		s = r*vec3(sin(theta)*cos(phi), sin(theta)*sin(phi), cos(theta));
		s += t;

		orbitTrap.xyz = min(orbitTrap.xyz, abs(s - vec3(sin(push.time), cos(push.time), sin(push.time/4.0)*cos(push.time/4.0))/1.75));
	}
	return min(0.5*log(r)*r/dr / reScale, 4.0);
}

const float maxBrightness = 1.35;
const float maxBrightnessR2 = maxBrightness*maxBrightness;
vec4 scaleColor(float si, vec3 col) {
	col *= pow(1.0 - si/float(maxIterations), 0.9);
	if(dot(col, col) > maxBrightnessR2) {
		col = maxBrightness*normalize(col);
	}
	return vec4(col, 1.0);
}

const float maxDistance = 100.0;
const float hitDistance = epsilon;
const float minTravel = 0.5;
vec4 castRay(vec3 position, vec3 direction)
{
	position += minTravel * direction;
	float travel = minTravel;
	for(int i = 0; i < maxIterations; i++) {
		int deMaxIter = int(6.0 * pow(1.0 - travel/maxDistance, 0.8) + 3.0);
		float dist = distanceEstimator(position, deMaxIter);

		float adjustedHitDistance = hitDistance;
		if(dist <= adjustedHitDistance) {
			vec4 color = orbitTrap;
			gradient = (vec3(distanceEstimator(position + epsilon*dirX, deMaxIter), distanceEstimator(position + epsilon*dirY, deMaxIter), distanceEstimator(position + epsilon*dirZ, deMaxIter))-dist)/epsilon;
			return scaleColor(i, color.xyz);
		}

		position += (0.95*dist)*direction;
		travel += dist;
		if(travel >= maxDistance) break;
	}
	return vec4(0.0, 0.0, 0.0, 1.0);
}

const float fov = (pi/1.75) / 2.0;
const float fovY = sin(fov);
void main(void) {
	float fovX = push.aspectRatio * fovY;
	vec3 direction = normalize(vec3(coord.x*fovX, -coord.y*fovY, 1.0));
	direction = rotateByQuaternion(direction, push.cameraQuaternion);

	fragColor = castRay(push.cameraPosition, direction);
}