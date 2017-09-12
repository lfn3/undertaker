package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ToShortFunction<T> {
    short applyAsShort(T value);
}
