<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns="http://www.w3.org/TR/xhtml1/strict">
<xsl:output
   method="xml"
   indent="yes"
   encoding="UTF-8"
   doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
   doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
   omit-xml-declaration="yes"
/>
<xsl:template match="/">
    <xsl:variable name="num_test_cases" select="count(/omni_xcodeml_test_report/test_case)"/>
    <xsl:variable name="num_failed_test_cases" select="count(/omni_xcodeml_test_report/test_case[@result != 'true'])"/>
    <xsl:variable name="num_ok_test_cases" select="$num_test_cases - $num_failed_test_cases"/>
  <html xmlns="http://www.w3.org/1999/xhtml">
  <head>
      <title>
          OMNI XcodeML Test report
      </title>
      <style type="text/css">
      table,
      th,
      td {
        padding: 10px;
        border: 1px solid black;
        border-collapse: collapse;
      }
    </style>
  </head>
  <body>
      <table>
          <tr>
              <td colspan="2"><b>Summary</b></td>
          </tr>
          <tr>
              <td><b>Result: </b></td>
              <xsl:if test="/omni_xcodeml_test_report/@result = 'true'">
                    <td style="color: green; font-weight: bold;">SUCCESS</td>
              </xsl:if>
              <xsl:if test="/omni_xcodeml_test_report/@result != 'true'">
                    <td style="color: red; font-weight: bold;">FAILED</td>
              </xsl:if>
          </tr>
          <tr>
              <td><b>Total number of testcases: </b></td><td> <xsl:value-of select="$num_test_cases"/></td>
          </tr>
          <tr>
             <td><b>Successful testcases: </b></td><td style="color: green;"> <xsl:value-of select="$num_ok_test_cases"/></td>
          </tr>
          <tr>
              <td>
                  <a href="#failed_testcases_table"><b>Failed testcases: </b></a>
              </td>
              <td style="color: red;"> <xsl:value-of select="$num_failed_test_cases"/></td>
          </tr>
          <tr>
              <td><b>Total duration: (seconds) </b></td>
              <td> <xsl:value-of select="number(/omni_xcodeml_test_report/@duration) div 1000000"/></td>
          </tr>
      </table>
      <br/>
      <xsl:if test="$num_failed_test_cases > 0">
      <a id="failed_testcases_table">
      <table>
          <tr>
              <td colspan="2"><b>Failed testcases</b></td>
          </tr>
          <tr>
              <td><b>#</b></td> <td><b>Name</b></td>
          </tr>

          <xsl:for-each select="/omni_xcodeml_test_report/test_case[@result != 'true']">
            <tr>
              <td><xsl:value-of select="position()"/></td>
                <td style="color: red;"><a href="#{@name}"><xsl:value-of select="@name"/></a></td>
            </tr>
          </xsl:for-each>
      </table>
      </a>
      </xsl:if>
      <br/>
      <table>
          <tr>
              <td colspan="3"><b>Testcases by duration</b></td>
          </tr>
          <tr>
              <td><b>#</b></td> <td><b>Name</b></td> <td><b>Duration (seconds)</b></td>
          </tr>

          <xsl:for-each select="/omni_xcodeml_test_report/test_case">
              <xsl:sort select="number(@duration)" data-type="number" order="descending"/>
            <tr>
              <td><xsl:value-of select="position()"/></td>
                <td><a href="#{@name}"><xsl:value-of select="@name"/></a></td>
              <td><xsl:value-of select="number(@duration) div 1000000"/></td>
            </tr>
          </xsl:for-each>
      </table>

      <br/>
      <xsl:for-each select="/omni_xcodeml_test_report/test_case[@result = 'false']">
          <a id="{@name}">
          <table>
              <tr>
                  <td colspan="4" style="color: red;"><b><xsl:value-of select="@name"/></b></td>
              </tr>
              <xsl:if test="@exception != 'true'">
              <tr>
                  <td colspan="4" style="color: red; font-weight: bold;">Exception</td>
              </tr>
              <tr>
                  <td colspan="4" style="color: red;"><xsl:value-of select="@exception"/></td>
              </tr>

              </xsl:if>
              <xsl:for-each select="./stage[@result != 'true']">
              <tr>
                  <td>Stage: </td> <td style="color: red;"><xsl:value-of select="@name"/></td>
                  <td>Duration: </td> <td><xsl:value-of select="number(@duration) div 1000000"/></td>
              </tr>
              <xsl:if test="@arguments">
              <tr> <td colspan="4" >Args:</td> </tr>
              <tr><td colspan="4" ><xsl:value-of select="@arguments"/></td></tr>
              </xsl:if>
              <xsl:if test="@error_log">
              <tr> <td colspan="4" >Stdout:</td> </tr>
              <tr><td colspan="4" ><xsl:value-of select="@error_log"/></td></tr>
              </xsl:if>
              </xsl:for-each>
          </table>
          </a>
      </xsl:for-each>
  </body>
  </html>
</xsl:template>
</xsl:stylesheet>