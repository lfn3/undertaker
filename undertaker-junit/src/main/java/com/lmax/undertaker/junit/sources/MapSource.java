package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Generator;
import com.lmax.undertaker.junit.Source;

import java.util.Map;
import java.util.function.BiFunction;

public interface MapSource {
    <K, V> Map<K, V> getMap(Generator<K> keyGenerator, Generator<V> valueGenerator);
    <K, V> Map<K, V> getMap(Generator<K> keyGenerator, BiFunction<Source, K, V> valueGenerator);
}
