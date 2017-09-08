package com.lmax.undertaker.junit.generators;

public interface DoubleGen {
    double getDouble();
    double getDouble(double max);
    double getDouble(double min, double max);
}
