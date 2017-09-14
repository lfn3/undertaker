package com.lmax.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToBoolFunction<T> {
    boolean applyAsBoolean(T value);
}
