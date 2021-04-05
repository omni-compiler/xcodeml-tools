package xcodeml.c.decompile;

import xcodeml.util.XmException;
import xcodeml.c.obj.XcNode;
import xcodeml.c.util.XmcWriter;

/**
 * Internal object represents null for expression.
 */
public class XcNullExpr extends XcObj implements XcExprObj {


    public static XcNullExpr createXcNullExpr()
    {
        return new XcNullExpr();
    }

    @Override
    public void appendCode(XmcWriter w) throws XmException
    {
        return;
    }

    @Override
    public void addChild(XcNode child)
    {
    }

    @Override
    public void checkChild()
    {
    }

    @Override
    public XcNode[] getChild()
    {
        return null;
    }

    @Override
    public void setChild(int index, XcNode child)
    {
    }
}
