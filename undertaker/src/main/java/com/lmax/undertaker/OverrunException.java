package com.lmax.undertaker;

public class OverrunException extends Exception {
    public OverrunException(IndexOutOfBoundsException cause) {
        super(cause);
    }
}
