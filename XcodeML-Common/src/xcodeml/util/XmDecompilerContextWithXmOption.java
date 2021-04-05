package xcodeml.util;

/**
 * Decompiler Context.
 */
public abstract class XmDecompilerContextWithXmOption implements XmDecompilerContext
{
    final IXmOption xmOption;

    protected XmDecompilerContextWithXmOption(IXmOption xmOption)
    {
        this.xmOption = xmOption;
    }

    public IXmOption getXmOption()
    {
        return xmOption;
    }
}
