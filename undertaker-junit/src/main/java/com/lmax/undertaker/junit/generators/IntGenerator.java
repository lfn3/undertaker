package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToIntFunction;

@FunctionalInterface
public interface IntGenerator extends ToIntFunction<Source> {
}
