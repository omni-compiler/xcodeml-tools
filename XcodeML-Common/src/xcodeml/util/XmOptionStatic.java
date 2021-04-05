package xcodeml.util;

import java.util.ArrayList;

/**
 * Decompile option.
 * @deprecated Not thread-safe.
 */
@Deprecated
public class XmOptionStatic implements IXmOption
{
    public XmOptionStatic()
    {
    }

    /**
     * Sets compiler to or not to suppress to write line directives.
     *
     * @param enable true then compiler suppress to write line directives.
     */
    @Override
    public void setIsSuppressLineDirective(boolean enable)
    {
        XmOption.setIsSuppressLineDirective(enable);
    }

    /**
     * Checks does decompiler suppress line directives.
     *
     * @return true if compiler suppress to write line directives.
     */
    @Override
    public boolean isSuppressLineDirective()
    {
        return XmOption.isSuppressLineDirective();
    }

    /**
     * Sets compiler to or not to translate XcalableMP directive.
     *
     * @param enable true then translate XcalableMP directive.
     */
    @Override
    public void setIsXcalableMP(boolean enable)
    {
        XmOption.setIsXcalableMP(enable);
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    @Override
    public boolean isXcalableMP()
    {
        return XmOption.isXcalableMP();
    }

    /**
     * Sets compiler to or not to translate XcalableMP-threads directive.
     */
    @Override
    public void setIsXcalableMPthreads(boolean enable)
    {
        XmOption.setIsXcalableMPthreads(enable);
    }

    /**
     * Checks does compiler translate XcalableMP-threads directive.
     */
    @Override
    public boolean isXcalableMPthreads()
    {
        return XmOption.isXcalableMPthreads();
    }

    /**
     * Sets compiler to or not to translate XcalableMP-GPU directive.
     */
    @Override
    public void setIsXcalableMPGPU(boolean enable)
    {
        XmOption.setIsXcalableMPGPU(enable);
    }

    /**
     * Checks does compiler translate XcalableMP-GPU directive.
     */
    @Override
    public boolean isXcalableMPGPU()
    {
        return XmOption.isXcalableMPGPU();
    }

    /**
     * Sets whether the compiler supports asynchronous communications or not.
     */
    @Override
    public void setIsAsync(boolean enable)
    {
        XmOption.setIsAsync(enable);
    }

    /**
     * Checks if the compiler supports asynchronous communications.
     */
    @Override
    public boolean isAsync()
    {
        return XmOption.isAsync();
    }

    @Override
    public void setIsXcalableACC(boolean enable)
    {
        XmOption.setIsXcalableACC(enable);
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    @Override
    public boolean isXcalableACC()
    {
        return XmOption.isXcalableACC();
    }

    /**
     * Sets compiler to or not to use tlog for MPI.
     */
    @Override
    public void setTlogMPIisEnable(boolean enable)
    {
        XmOption.setTlogMPIisEnable(enable);
    }

    /**
     * Sets compiler to or not to use one-sided functions in Fortran.
    */
    @Override
    public void setFonesided(boolean enable)
    {
        XmOption.setFonesided(enable);
    }

    @Override
    public boolean isFonesided()
    {
        return XmOption.isFonesided();
    }

    /**
     * Checks does compiler use tlog for MPI.
     */
    @Override
    public boolean tlogMPIisEnable()
    {
        return XmOption.tlogMPIisEnable();
    }

    /**
     * Sets compiler to or not to translate OpenMP directive.
     *
     * @param enable true then translate OpenMP directive.
     */
    @Override
    public void setIsOpenMP(boolean enable)
    {
        XmOption.setIsOpenMP(enable);
    }

    /**
     * Sets compiler to or not to translate OpenMP only target directive.
     *
     * @param enable true then translate OpenMP only target directive.
     */
    @Override
    public void setIsOpenMPonlyTarget(boolean enable)
    {
        XmOption.setIsOpenMPonlyTarget(enable);
    }

    /**
     * Checks does compiler translate OpenMP directive.
     *
     * @return true if compiler translate OpenMP directive.
     */
    @Override
    public boolean isOpenMP()
    {
        return XmOption.isOpenMP();
    }

    /**
     * Checks does compiler translate OpenMP only target directive.
     *
     * @return true if compiler translate OpenMP only target directive.
     */
    @Override
    public boolean isOpenMPOnlyTarget()
    {
        return XmOption.isOpenMPOnlyTarget();
    }

    /**
     * Sets compiler to or not to translate coarrays
     *
     * @param enable true then translate coarrays.
     */
    @Override
    public void setIsCoarray(boolean enable)
    {
        XmOption.setIsCoarray(enable);
    }

    /**
     * Checks does compiler translate coarrays.
     *
     * @return true if compiler translate coarrays.
     */
    @Override
    public boolean isCoarray()
    {
        return XmOption.isCoarray();
    }

    /**
     * Adds a name of coarray runtime library
     *
     * @param name added to the list of coarray runtime library
     */
    @Override
    public void addToCoarrayEntryNames(String name)
    {
        XmOption.addToCoarrayEntryNames(name);
    }

    /**
     * Gets the list of coarray runtime library
     *
     * @return the list of coarray runtime library
     */
    @Override
    public ArrayList<String> getCoarrayEntryNames()
    {
        return XmOption.getCoarrayEntryNames();
    }

    /**
     * Return true if debug output enabled.
     */
    @Override
    public boolean isDebugOutput()
    {
        return XmOption.isDebugOutput();
    }

    /**
     * Set debug output.
     */
    @Override
    public void setDebugOutput(boolean enable)
    {
        XmOption.setDebugOutput(enable);
    }

    /**
     * Set language
     */
    @Override
    public void setLanguage(XmLanguage lang)
    {
        XmOption.setLanguage(lang);
    }

    /**
     * Get language
     */
    @Override
    public XmLanguage getLanguage()
    {
        return XmOption.getLanguage();
    }

    /**
     * Set name of the main function
     */
    @Override
    public void setMainName(String main_name)
    {
        XmOption.setMainName(main_name);
    }

    /**
     * Get name of the main function
     */
    @Override
    public String getMainName()
    {
        return XmOption.getMainName();
    }

    /**
     * Return if the language is C
     */
    @Override
    public boolean isLanguageC()
    {
        return XmOption.isLanguageC();
    }

    /**
     * Return if the language is Fortran
     */
    @Override
    public boolean isLanguageF()
    {
        return XmOption.isLanguageF();
    }

    /**
     * Return compiler vendor constant. (COMP_VENDOR_*)
     */
    @Override
    public int getCompilerVendor()
    {
        return XmOption.getCompilerVendor();
    }

    /**
     * Set compiler vendor constant. (COMP_VENDOR_*)
     */
    @Override
    public void setCompilerVendor(int vendor)
    {
        XmOption.setCompilerVendor(vendor);
    }

    /**
     * Get if or not IO statements are transformed to atomic operation.
     */
    @Override
    public boolean isAtomicIO()
    {
        return XmOption.isAtomicIO();
    }

    /**
     * Set if or not IO statements are transformed to atomic operation.
     */
    @Override
    public void setIsAtomicIO(boolean atomicIO)
    {
        XmOption.setIsAtomicIO(atomicIO);
    }

    /**
     * Set/get suboption -fcoarray-use-statement (boolean)
     */
    @Override
    public void setCoarrayUseStatement(boolean coarrayUseStatement)
    {
        XmOption.setCoarrayUseStatement(coarrayUseStatement);
    }

    @Override
    public boolean coarrayUseStatement()
    {
        return XmOption.coarrayUseStatement();
    }

    /**
     * Sets compiler to ensure left to right evaluation of mathematical
     * expression.
     *
     * @param enable true then compiler add parenthesis to ensure evaluation.
     */
    @Override
    public void setAddPar(boolean enable)
    {
        XmOption.setAddPar(enable);
    }

    /**
     * Checks if the add parenthesis options is enabled.
     *
     * @return true if add parenthesis option is enabled.
     */
    @Override
    public boolean isAddParEnabled()
    {
        return XmOption.isAddParEnabled();
    }

    @Override
    public void setPointerScalarSize(int value)
    {
	    XmOption.setPointerScalarSize(value);
    }

    @Override
    public int getPointerScalarSize()
    {
	    return XmOption.getPointerScalarSize();
    }

    @Override
    public void setPointerArraySize(int value)
    {
  	    XmOption.setPointerArraySize(value);
    }

    @Override
    public int getPointerArraySize()
    {
  	    return XmOption.getPointerArraySize();
	}

    @Override
    public void setPointerDiffSize(int value)
    {
	    XmOption.setPointerDiffSize(value);
    }

    @Override
    public int getPointerDiffSize()
    {
     	return XmOption.getPointerDiffSize();
    }
}
