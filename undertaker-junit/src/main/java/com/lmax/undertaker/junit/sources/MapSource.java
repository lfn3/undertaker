package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Generator;
import javafx.util.Pair;

import java.util.Map;

public interface MapSource {
    <K, V> Map<K, V> getMap(Generator<Pair<K, V>> entryGenerator);
}
