package com.lmax.undertaker.junit.sources;

public interface EnumSource {
    <T extends Enum> T getEnum(Class<T> enumClass);
}
