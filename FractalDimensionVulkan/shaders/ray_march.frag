/*
This file is part of FractalDimension

FractalDimension is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FractalDimension is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with FractalDimension. If not, see <https://www.gnu.org/licenses/>.
*/

#version 450
layout (location = 0) in vec2 coord;

layout (location = 0) out vec4 fragColor;

layout (push_constant) uniform Push
{
	vec4 cameraQuaternion;
	float time;
	float aspectRatio;
	int deType;

	vec3 reactiveBass;
    vec3 reactiveMids;
    vec3 reactiveHigh;
    
    vec3 smoothBass;
    vec3 smoothMids;
    vec3 smoothHigh;
} push;

const float pi = 3.14159265358;
const float tau = 2.0*pi;
const float e = 2.718281828;
const float epsilon = 0.00005;
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

float bound(float x, float b)
{
	return mod(x + b, 2.0*b) - b;
}
float boundReflect(float x, float b)
{
	float r = mod(x + b, 4.0*b);
	if(r < 2.0*b)
	{
		return r - b;
	} else
	{
		return 3.0*b - r;
	}
}

vec3 gradient;
vec4 orbitTrap;
float distanceEstimator(vec3 t)
{
	orbitTrap = vec4(1.0, 1.0, 1.0, 1.0);

	//*/
	// Mandelbox
	if(push.deType == 1)
	{
		const int maxIterations = 6;
		const float reScale = 4.5;
		t *= reScale;
		vec3 s = t;
		const float mandelboxScale = 0.25*cos(0.35 * push.time) - 2.1;
		float DEfactor = 1.0;
		float r2 = 1.0;
		const float maxR2 = 12.0;
		const float BVR = sqrt(maxR2);
		for (int i = 0; i < maxIterations; i++)
		{
			if(s.x>1.0){s.x=2.0-s.x;}else if(s.x<-1.0){s.x=-2.0-s.x;}
			if(s.y>1.0){s.y=2.0-s.y;}else if(s.y<-1.0){s.y=-2.0-s.y;}
			if(s.z>1.0){s.z=2.0-s.z;}else if(s.z<-1.0){s.z=-2.0-s.z;}

			r2 = dot(s, s);
			if (r2 < 0.25) {
				s *= 4.0;
				DEfactor *= 4.0;
			} else if(r2 < 1.0) {
				s /= r2;
				DEfactor /= r2;
			}

			orbitTrap.x = min(orbitTrap.x, length(s/BVR - push.reactiveBass)/2.0);
			orbitTrap.y = min(orbitTrap.y, length(s/BVR - push.reactiveMids)/2.0);
			orbitTrap.z = min(orbitTrap.z, length(s/BVR - push.reactiveHigh)/2.0);

			s = s*mandelboxScale + t;
			DEfactor = DEfactor*abs(mandelboxScale) + 1.0;
		
			if(r2 > maxR2) break;
		}
		return (length(s)-BVR)/abs(DEfactor) / reScale;
	}
	// Mandelbulb
	else if(push.deType == 2)
	{
		// Mandelbulb
		const int maxIterations = 4;
		const float reScale = 2.25;
		t *= reScale;
		t = vec3(boundReflect(t.x, 10.0), boundReflect(t.y, 10.0), boundReflect(t.z, 10.0));
		vec3 s = t;
		float power = 9. + 2.0*boundReflect(0.12*push.time + 1.0, 1.0);
		float dr = 1.0;
		float r = 0.0;

		mat3 colorRotato = buildRot3(normalize(push.smoothMids), 0.8*push.time);

		for(int i = 0; i < maxIterations; i++)
		{
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

			orbitTrap.xyz = min(orbitTrap.xyz, abs((s - (push.reactiveHigh + push.reactiveBass)/1.75) * colorRotato));
		}
		return min(0.5*log(r)*r/dr, 3.5) / reScale;
	}
	else if(push.deType == 3)
	{
		const int maxIterations = 4;
		const float reScale = 0.45;
		t = reScale*t;
		vec3 s = t;

		float anim = 1.275 + 0.085*sin(0.5*push.time);
		float scale = 1.0;
		float theta = 0.25 * push.time;
		float ct = cos(theta);
		float st = sin(theta);
		mat2 rotato = mat2(ct, st, -st, ct);

		for(int i = 0; i < maxIterations; i++)
		{
			if (i == 2)
			{
				s.xy *= rotato;
			}

			s = -1.0 + 2.0*fract(0.5*s + 0.5);

			float r2 = dot(s,s);
		
			float k = anim/r2;
			s *= k;
			scale *= k;

			orbitTrap.x = min(orbitTrap.x, length(s/2.0 - push.reactiveBass)/2.0);
			orbitTrap.y = min(orbitTrap.y, length(s/2.0 - push.reactiveMids)/2.0);
			orbitTrap.z = min(orbitTrap.z, length(s/2.0 - push.reactiveHigh)/2.0);
		}
	
		return (0.25*abs(s.z)/scale) / reScale;
	}
	else if(push.deType == 4)
	{
		const int maxIterations = 4;

		const float reScale = 1.4;
		t *= reScale;
		vec3 s = t;

		s = vec3(boundReflect(s.x, 7.5), boundReflect(s.y, 7.5), boundReflect(s.z, 7.5));

		s = s + 0.5; //center it by changing position and scale
		float xx=abs(s.x-0.5)-0.5, yy=abs(s.y-0.5)-0.5, zz=abs(s.z-0.5)-0.5;
		float d1=max(xx,max(yy,zz)); //distance to the box
		float d=d1; //current computed distance
		float p=1.0;
		float mengerScale = 3.0;
		float halfScale = mengerScale / 2.0;

		orbitTrap.xyz = abs(vec3(xx/1.25, yy/1.25, zz/1.25));

		float theta = 0.58*sin(0.23*push.time);
		mat3 rotato = buildRot3(normalize(push.smoothMids), theta);

		for (int i = 0; i < maxIterations; i++)
		{
			p *= mengerScale;
			float xa = mod(s.x*p, mengerScale);
			float ya = mod(s.y*p, mengerScale);
			float za = mod(s.z*p, mengerScale);

			float xx=0.5-abs(xa-halfScale), yy=0.5-abs(ya-halfScale), zz=0.5-abs(za-halfScale);
			d1=min(max(xx,zz),min(max(xx,yy),max(yy,zz))) / p; //distance inside the 3 axis-aligned square tubes

			d=max(d,d1); //intersection

			if (i % 2 == 1)
			{
				const float rat = 0.815;
				vec3 q = vec3(xx, yy, zz);
				vec3 col = abs(q/1.25);
				orbitTrap.xyz = rat*orbitTrap.xyz + (1.0 - rat)*col;
			}

			s *= rotato;
		}
		return d/reScale;
	}
	else if(push.deType == 5)
	{
		const int maxIterations = 8;
		const float scale = 2.0;
		const float reScale = 1.85;

		t *= reScale;
		t = vec3(boundReflect(t.x, 9.0), boundReflect(t.y, 9.0), boundReflect(t.z, 9.0));
		vec3 s = t;
		const vec3 center = vec3(sqrt(0.5), sqrt(0.3), sqrt(0.2));
		float r2 = dot(s, s);
		float DEfactor = 1.0;

		float theta = 0.3*push.time;
		mat3 rotato1 = buildRot3(normalize(push.smoothHigh), theta);
		theta = 0.37*sin(1.25*push.time);
		mat3 rotato2 = buildRot3(normalize(push.smoothMids), theta);

		for(int i = 0; i < maxIterations && r2 < 1000.0; i++)
		{
			s *= rotato1;

			if(s.x+s.y<0.0){float x1=-s.y;s.y=-s.x;s.x=x1;}
			if(s.x+s.z<0.0){float x1=-s.z;s.z=-s.x;s.x=x1;}
			if(s.y+s.z<0.0){float y1=-s.z;s.z=-s.y;s.y=y1;}

			s *= rotato2;

			s = scale*s - (scale - 1.0)*center;
			r2 = dot(s, s);

			orbitTrap.x = min(orbitTrap.x, length(s - push.reactiveBass)/2.0);
			orbitTrap.y = min(orbitTrap.y, length(s - push.reactiveMids)/2.0);
			orbitTrap.z = min(orbitTrap.z, length(s - push.reactiveHigh)/2.0);

			DEfactor *= scale;
		}
		return (sqrt(r2) - 2.0) / DEfactor / reScale;
	}
	else
	{
		return 1000.0;
	}
}

const float maxBrightness = 1.35;
const float maxBrightnessR2 = maxBrightness*maxBrightness;
vec4 scaleColor(float distanceRatio, float iterationRatio, vec3 col)
{
	col *= pow(1.0 - distanceRatio, 1.2) * pow(1.0 - iterationRatio, 3.5);
	if(dot(col, col) > maxBrightnessR2)
	{
		col = maxBrightness*normalize(col);
	}
	return vec4(col, 1.0);
}

vec4 castRay(vec3 position, vec3 direction)
{
	const int maxIterations = 150;
	const float maxDistance = 60.0;
	const float hitDistance = epsilon;
	float minTravel = 0.2;
	if(push.deType ==  1)
	{
		minTravel = minTravel + max(0.0, -0.8*cos(0.15 * push.time));
	}

	position += minTravel * direction;
	float travel = minTravel;
	for(int i = 0; i < maxIterations; i++)
	{
		float dist = distanceEstimator(position);

		float adjustedHitDistance = hitDistance;
		if(dist <= adjustedHitDistance)
		{
			return scaleColor(travel/maxDistance, float(i)/float(maxIterations), orbitTrap.xyz);
		}

		position += (0.99*dist)*direction;
		travel += dist;
		if(travel >= maxDistance)
		{
			vec3 sinDir = sin(100.0*direction);
			vec3 base = vec3(exp(-3.0*length(sin(pi * push.reactiveBass + 1.0) - sinDir)), exp(-4.0*length(sin(e * push.reactiveMids + 1.3) - sinDir)), exp(-3.0*length(sin(9.6*push.reactiveHigh + 117.69420) - sinDir)));
			return vec4((push.deType == 0 ? 0.25 : 0.1) * base, 1.0);
		}
	}
	return vec4(0.0, 0.0, 0.0, 1.0);
}

void main(void)
{
	const float verticalFov = (pi/2.5) / 2.0;	// Roughly 70 degress vertical FOV
	const float fovY = tan(verticalFov);
	float fovX = push.aspectRatio * fovY;

	vec3 direction = normalize(vec3(coord.x*fovX, -coord.y*fovY, 1.0));
	direction = rotateByQuaternion(direction, push.cameraQuaternion);
	vec3 position = rotateByQuaternion(-dirZ, push.cameraQuaternion);

	//fragColor = castRay(-2.5 * direction, direction);
	fragColor = castRay(position, direction);
	//fragColor = castRay(push.deType == 2 ? (-2.5 * direction) : position, direction);
}