package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ToFloatFunction<T> {
    float applyAsFloat(T value);
}
