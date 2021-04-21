/*
 * @author Mikhail Zhigun
 */
package xcodeml.f.util;

public class NativeUtils
{
    public static class CppException extends Exception
    {
        public CppException(String msg)
        {
            super(msg);
        }
    }

    public static class Pointer
    {
        public long getAddress()
        {
            return address;
        }

        public final long address;

        public Pointer(long address)
        {
            this.address = address;
        }
    }
}
