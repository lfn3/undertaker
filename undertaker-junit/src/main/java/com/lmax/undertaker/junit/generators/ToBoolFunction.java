package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ToBoolFunction<T> {
    boolean applyAsBoolean(T value);
}
