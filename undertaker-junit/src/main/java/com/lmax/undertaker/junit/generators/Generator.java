package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

@FunctionalInterface
public interface Generator<T> extends Function<Source, T> {

}
