package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ToByteFunction<T> {
    byte applyAsByte(T value);
}
