package com.lmax.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToByteFunction<T> {
    byte applyAsByte(T value);
}
