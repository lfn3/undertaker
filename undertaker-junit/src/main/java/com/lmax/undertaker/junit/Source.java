package com.lmax.undertaker.junit;

import org.junit.rules.TestRule;
import com.lmax.undertaker.junit.generators.*;

public interface Source extends BoolGen, ByteGen, IntGen, ListGen, LongGen, LongArrayGen, TestRule
{
    long pushInterval(String intervalName);
    void popInterval(long intervalId, Object generatedValue);
}
