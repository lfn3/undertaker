package com.lmax.undertaker;

import clojure.lang.ExceptionInfo;

public class UndertakerDebugException extends Exception {
    public UndertakerDebugException(ExceptionInfo cause) {
        super(cause);
    }
}
