package undertaker;

public interface Source {
    byte getByte();
    byte[] getBytes(int number);

    long pushInterval(String intervalName);
    void popInterval(long intervalId, Object generatedValue);
    Interval getIntervals();
}
