package com.lmax.undertaker.junit.sources;

public interface FloatSource {
    float getFloat();
    float getFloat(float max);
    float getFloat(float min, float max);
}