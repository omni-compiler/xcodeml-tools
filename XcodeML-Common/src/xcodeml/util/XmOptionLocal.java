package xcodeml.util;

import java.util.ArrayList;

/**
 * Decompile option.
 */
public class XmOptionLocal implements IXmOption
{
    /** if suppress to write line directives */
    private boolean _suppressLineDirective = false;

    /** if compiling Xcalable MP is enabled */
    private boolean _xcalableMP = false;
    private boolean _xcalableMPthreads = false;
    private boolean _xcalableMPGPU = false;
    private boolean _xcalableMPasync = false;

    private boolean _tlog = false;
    private boolean _Fonesided = false;

    /** if compiling XcalableACC is enabled */
    private boolean _xcalableACC = false;

    /** if compiling OpenMP is enabled */
    private boolean _openMP = false;

    /** if compiling OpenMP only target is enabled */
    private boolean _openMPonlyTarget = false;

    /** if compiling coarray is enabled */
    private boolean _coarray = false;

    /** if debug output is enabled */
    private boolean _debugOutput = false;

    /** ensure left to right evaluation of mathematical expression **/
    private boolean _addPar = false;

    /** target language ID */
    private XmLanguage _language = XmLanguage.C;

    /** Name of tht main function */
    private String _mainName = "";

    /** if transforming Fortran IO statement as atomic operation */
    private boolean _isAtomicIO = false;

    /** backend compiler vendor */
    private int _compilerVendor = COMP_VENDOR_GNU;

    /** if supressing generation of USE statement for coarray runtime
     *  (TEMPORARY)
     */
    private boolean _coarrayUseStatement = false;
    private ArrayList<String> _coarrayEntryNames = new ArrayList<>();

    private int _pointer_scalar_size = 8;  // This value for gcc-8.3.0
    private int _pointer_array_size  = 40; // This value for gcc-8.3.0
    private int _pointer_diff_size   = 24; // This value for gcc-8.3.0

    public XmOptionLocal()
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
        _suppressLineDirective = enable;
    }

    /**
     * Checks does decompiler suppress line directives.
     *
     * @return true if compiler suppress to write line directives.
     */
    @Override
    public boolean isSuppressLineDirective()
    {
        return _suppressLineDirective;
    }

    /**
     * Sets compiler to or not to translate XcalableMP directive.
     *
     * @param enable true then translate XcalableMP directive.
     */
    @Override
    public void setIsXcalableMP(boolean enable)
    {
        _xcalableMP = enable;
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    @Override
    public boolean isXcalableMP()
    {
        return _xcalableMP;
    }

    /**
     * Sets compiler to or not to translate XcalableMP-threads directive.
     */
    @Override
    public void setIsXcalableMPthreads(boolean enable)
    {
        _xcalableMPthreads = enable;
    }

    /**
     * Checks does compiler translate XcalableMP-threads directive.
     */
    @Override
    public boolean isXcalableMPthreads()
    {
        return _xcalableMPthreads;
    }

    /**
     * Sets compiler to or not to translate XcalableMP-GPU directive.
     */
    @Override
    public void setIsXcalableMPGPU(boolean enable)
    {
        _xcalableMPGPU = enable;
    }

    /**
     * Checks does compiler translate XcalableMP-GPU directive.
     */
    @Override
    public boolean isXcalableMPGPU()
    {
        return _xcalableMPGPU;
    }

    /**
     * Sets whether the compiler supports asynchronous communications or not.
     */
    @Override
    public void setIsAsync(boolean enable)
    {
        _xcalableMPasync = enable;
    }

    /**
     * Checks if the compiler supports asynchronous communications.
     */
    @Override
    public boolean isAsync()
    {
        return _xcalableMPasync;
    }

    @Override
    public void setIsXcalableACC(boolean enable)
    {
        _xcalableACC = enable;
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    @Override
    public boolean isXcalableACC()
    {
        return _xcalableACC;
    }

    /**
     * Sets compiler to or not to use tlog for MPI.
     */
    @Override
    public void setTlogMPIisEnable(boolean enable)
    {
        _tlog = enable;
    }

    /**
     * Sets compiler to or not to use one-sided functions in Fortran.
    */
    @Override
    public void setFonesided(boolean enable)
    {
        _Fonesided = enable;
    }

    @Override
    public boolean isFonesided()
    {
        return _Fonesided;
    }

    /**
     * Checks does compiler use tlog for MPI.
     */
    @Override
    public boolean tlogMPIisEnable()
    {
        return _tlog;
    }

    /**
     * Sets compiler to or not to translate OpenMP directive.
     *
     * @param enable true then translate OpenMP directive.
     */
    @Override
    public void setIsOpenMP(boolean enable)
    {
        _openMP = enable;
    }

    /**
     * Sets compiler to or not to translate OpenMP only target directive.
     *
     * @param enable true then translate OpenMP only target directive.
     */
    @Override
    public void setIsOpenMPonlyTarget(boolean enable)
    {
     	_openMPonlyTarget = enable;
    }

    /**
     * Checks does compiler translate OpenMP directive.
     *
     * @return true if compiler translate OpenMP directive.
     */
    @Override
    public boolean isOpenMP()
    {
        return _openMP;
    }

    /**
     * Checks does compiler translate OpenMP only target directive.
     *
     * @return true if compiler translate OpenMP only target directive.
     */
    @Override
    public boolean isOpenMPOnlyTarget()
    {
        return _openMPonlyTarget;
    }

    /**
     * Sets compiler to or not to translate coarrays
     *
     * @param enable true then translate coarrays.
     */
    @Override
    public void setIsCoarray(boolean enable)
    {
        _coarray = enable;
    }

    /**
     * Checks does compiler translate coarrays.
     *
     * @return true if compiler translate coarrays.
     */
    @Override
    public boolean isCoarray()
    {
        return _coarray;
    }

    /**
     * Adds a name of coarray runtime library
     *
     * @param name added to the list of coarray runtime library
     */
    @Override
    public void addToCoarrayEntryNames(String name)
    {
        _coarrayEntryNames.add(name);
    }

    /**
     * Gets the list of coarray runtime library
     *
     * @return the list of coarray runtime library
     */
    @Override
    public ArrayList<String> getCoarrayEntryNames()
    {
        return _coarrayEntryNames;
    }

    /**
     * Return true if debug output enabled.
     */
    @Override
    public boolean isDebugOutput()
    {
        return _debugOutput;
    }

    /**
     * Set debug output.
     */
    @Override
    public void setDebugOutput(boolean enable)
    {
        _debugOutput = enable;
    }

    /**
     * Set language
     */
    @Override
    public void setLanguage(XmLanguage lang)
    {
        _language = lang;
    }

    /**
     * Get language
     */
    @Override
    public XmLanguage getLanguage()
    {
        return _language;
    }

    /**
     * Set name of the main function
     */
    @Override
    public void setMainName(String main_name)
    {
        _mainName = main_name;
    }

    /**
     * Get name of the main function
     */
    @Override
    public String getMainName()
    {
        return _mainName;
    }

    /**
     * Return if the language is C
     */
    @Override
    public boolean isLanguageC()
    {
        return _language.equals(XmLanguage.C);
    }

    /**
     * Return if the language is Fortran
     */
    @Override
    public boolean isLanguageF()
    {
        return _language.equals(XmLanguage.F);
    }

    /**
     * Return compiler vendor constant. (COMP_VENDOR_*)
     */
    @Override
    public int getCompilerVendor()
    {
        return _compilerVendor;
    }

    /**
     * Set compiler vendor constant. (COMP_VENDOR_*)
     */
    @Override
    public void setCompilerVendor(int vendor)
    {
        _compilerVendor = vendor;
    }

    /**
     * Get if or not IO statements are transformed to atomic operation.
     */
    @Override
    public boolean isAtomicIO()
    {
        return _isAtomicIO || _compilerVendor == COMP_VENDOR_INTEL;
    }

    /**
     * Set if or not IO statements are transformed to atomic operation.
     */
    @Override
    public void setIsAtomicIO(boolean atomicIO)
    {
        _isAtomicIO = atomicIO;
    }

    /**
     * Set/get suboption -fcoarray-use-statement (boolean)
     */
    @Override
    public void setCoarrayUseStatement(boolean coarrayUseStatement)
    {
        _coarrayUseStatement = coarrayUseStatement;
    }

    @Override
    public boolean coarrayUseStatement()
    {
        return _coarrayUseStatement;
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
        _addPar = enable;
    }

    /**
     * Checks if the add parenthesis options is enabled.
     *
     * @return true if add parenthesis option is enabled.
     */
    @Override
    public boolean isAddParEnabled()
    {
        return _addPar;
    }

    @Override
    public void setPointerScalarSize(int value)
    {
	    _pointer_scalar_size = value;
    }

    @Override
    public int getPointerScalarSize()
    {
	    return _pointer_scalar_size;
    }

    @Override
    public void setPointerArraySize(int value)
    {
  	    _pointer_array_size = value;
    }

    @Override
    public int getPointerArraySize()
    {
  	    return _pointer_array_size;
	}

    @Override
    public void setPointerDiffSize(int value)
    {
	    _pointer_diff_size = value;
    }

    @Override
    public int getPointerDiffSize()
    {
     	return _pointer_diff_size;
    }
}
