package xcodeml.c.util;

import xcodeml.util.IXmOption;
import xcodeml.util.XmDecompilerContextWithXmOption;
import xcodeml.util.XmOptionStatic;

public class XmcDecompilerContext extends XmDecompilerContextWithXmOption
{
    @Deprecated
    public XmcDecompilerContext()
    {
        super(new XmOptionStatic());
    }

    public XmcDecompilerContext(IXmOption xmOption)
    {
        super(xmOption);
    }

    @Override
    public void setProperty(String key, Object value)
    {
    }
}
