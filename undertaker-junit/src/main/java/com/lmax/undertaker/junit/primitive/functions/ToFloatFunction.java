package com.lmax.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToFloatFunction<T> {
    float applyAsFloat(T value);
}
