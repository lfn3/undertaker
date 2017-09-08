package com.lmax.undertaker.junit.generators;

public interface DoubleGen {
    double getDouble();
    double getDouble(double max);
    double getDouble(double min, double max);

    double getRealDouble();
    double getRealDouble(double max);
    double getRealDouble(double min, double max);
}
