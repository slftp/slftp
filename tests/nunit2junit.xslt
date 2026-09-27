<?xml version="1.0" encoding="UTF-8"?>
<!--
  Converts a DUnitX NUnit 2.x style XML report (dunitx-results.xml) into the
  JUnit XML schema understood by GitLab CI test reports.

  NUnit input structure produced by DUnitX.Loggers.Xml.NUnit:
    test-results
      test-suite[@type='Assembly']
        results
          test-suite[@type='Namespace']
            results
              test-suite[@type='Fixture']
                results
                  test-case[@executed='True|False'][@result='Success|Failure|Error|Ignored']
                    failure/message, failure/stack-trace | reason/message

  Used by the testwin64 CI job via Convert-NUnitToJUnit.ps1.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" encoding="UTF-8"/>

  <xsl:template match="/test-results">
    <testsuites>
      <xsl:attribute name="name">slftpUnitTests</xsl:attribute>
      <xsl:attribute name="tests"><xsl:value-of select="@total"/></xsl:attribute>
      <xsl:attribute name="failures"><xsl:value-of select="@failures"/></xsl:attribute>
      <xsl:attribute name="errors"><xsl:value-of select="@errors"/></xsl:attribute>
      <xsl:attribute name="time"><xsl:value-of select="@time"/></xsl:attribute>
      <xsl:apply-templates select=".//test-suite[@type='Fixture']"/>
    </testsuites>
  </xsl:template>

  <xsl:template match="test-suite">
    <testsuite>
      <xsl:attribute name="name">
        <xsl:value-of select="concat(ancestor::test-suite[@type='Namespace'][1]/@name, '.', @name)"/>
      </xsl:attribute>
      <xsl:attribute name="tests"><xsl:value-of select="count(.//test-case)"/></xsl:attribute>
      <xsl:attribute name="failures"><xsl:value-of select="count(.//test-case[@result='Failure'])"/></xsl:attribute>
      <xsl:attribute name="errors"><xsl:value-of select="count(.//test-case[@result='Error'])"/></xsl:attribute>
      <xsl:attribute name="skipped"><xsl:value-of select="count(.//test-case[@executed='False'])"/></xsl:attribute>
      <xsl:attribute name="time"><xsl:value-of select="@time"/></xsl:attribute>
      <xsl:apply-templates select=".//test-case"/>
    </testsuite>
  </xsl:template>

  <xsl:template match="test-case">
    <testcase>
      <xsl:attribute name="classname">
        <xsl:value-of select="concat(ancestor::test-suite[@type='Namespace'][1]/@name, '.', ancestor::test-suite[@type='Fixture'][1]/@name)"/>
      </xsl:attribute>
      <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
      <xsl:attribute name="time"><xsl:value-of select="@time"/></xsl:attribute>
      <xsl:if test="@executed='False'">
        <skipped>
          <xsl:attribute name="message"><xsl:value-of select="reason/message"/></xsl:attribute>
        </skipped>
      </xsl:if>
      <xsl:if test="@executed='True' and @result='Failure'">
        <failure>
          <xsl:attribute name="message"><xsl:value-of select="failure/message"/></xsl:attribute>
          <xsl:value-of select="failure/stack-trace"/>
        </failure>
      </xsl:if>
      <xsl:if test="@executed='True' and @result='Error'">
        <error>
          <xsl:attribute name="message"><xsl:value-of select="failure/message"/></xsl:attribute>
          <xsl:value-of select="failure/stack-trace"/>
        </error>
      </xsl:if>
    </testcase>
  </xsl:template>

</xsl:stylesheet>
