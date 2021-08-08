#pragma once

#include <iso646.h>
#include <stdbool.h>
#include <stdint.h>

#define global     static
#define internal   static
#define persistent static

#define BETWEEN( x, a, b ) ( (x) > (a) && (x) < (b) )

int64_t  typedef s8;
int32_t  typedef s4;
int16_t  typedef s2;
int8_t   typedef s1;

uint64_t typedef u8;
uint32_t typedef u4;
uint16_t typedef u2;
uint8_t  typedef u1;

float    typedef f1;
double   typedef f2;
float    typedef f4;
double   typedef f8;

u1       typedef byte;
