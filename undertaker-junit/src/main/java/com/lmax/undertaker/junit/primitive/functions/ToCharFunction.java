package com.lmax.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToCharFunction<T> {
    char applyAsChar(T value);
}
