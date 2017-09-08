package com.lmax.undertaker.junit.generators;

public interface FloatGen {
    float getFloat();
    float getFloat(float max);
    float getFloat(float min, float max);
}
