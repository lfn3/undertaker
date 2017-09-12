package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ToCharFunction<T> {
    char applyAsChar(T value);
}
