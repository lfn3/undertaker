package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.List;
import java.util.function.Function;

public interface ListGen
{
    <T> List<T> getList(Function<Source, T> generator);
}
