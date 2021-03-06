<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet type="text/xml"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
		xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:h="urn:hl7-org:v3" xsi:schemaLocation="urn:hl7-org:v3 ../XSD/hg08_V08.xsd">
  <xsl:template match="/">
    <html>
      <head>
	<link rel="stylesheet" href="style.css" type="text/css" />
      </head>
      <body>
	<table class="main">
	  <tr><th>氏名</th><td><xsl:value-of select="//h:patientRole//h:patient/h:name" /></td></tr>
	  <tr><th>記号</th><td><xsl:value-of select="//h:patientRole//h:id[@root='1.2.392.200119.6.204']/@extension" /></td></tr>
	  <tr><th>番号</th><td><xsl:value-of select="//h:patientRole//h:id[@root='1.2.392.200119.6.205']/@extension" /></td></tr>
	  <tr><th>生年月日</th><td><xsl:value-of select="//h:patientRole//h:birthTime/@value" /></td></tr>
	  <tr><th>医療機関CD</th><td><xsl:value-of select="//h:representedOrganization/h:id/@extension" /></td></tr>
	  <tr><th>医療機関</th><td><xsl:value-of select="//h:representedOrganization//h:name" /></td></tr>
	  <tr><th>受診券番号</th><td><xsl:value-of select="//h:id[@root='1.2.392.200119.6.209.100263129']/@extension" /></td></tr>
	  <tr><th>利用券番号</th><td><xsl:value-of select="//h:id[@root='1.2.392.200119.6.210.100263129']/@extension" /></td></tr>
	  <tr><th>実施日</th><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.1002']/../h:effectiveTime/@value" /></td></tr>
	  <tr>
	    <th>支援レベル</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1020000001']/../h:value/@code = 1">(1) 積極的支援</xsl:when>
		<xsl:when test="//h:code[@code='1020000001']/../h:value/@code = 2">(2) 動機付支援</xsl:when>
		<xsl:when test="//h:code[@code='1020000001']/../h:value/@code = 3">(3) なし</xsl:when>
		<xsl:when test="//h:code[@code='1020000001']/../h:value/@code = 4">(4) 判定不能</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th>行動変容ステージ</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1020000002']/../h:value/@code = 1">(1) 意志なし</xsl:when>
		<xsl:when test="//h:code[@code='1020000002']/../h:value/@code = 2">(2) 意志あり 6か月以内</xsl:when>
		<xsl:when test="//h:code[@code='1020000002']/../h:value/@code = 3">(3) 意志あり 近いうち</xsl:when>
		<xsl:when test="//h:code[@code='1020000002']/../h:value/@code = 4">(4) 取り組み済 6か月未満</xsl:when>
		<xsl:when test="//h:code[@code='1020000002']/../h:value/@code = 5">(5) 取り組み済 6か月以上</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr><th>指導コース名</th><td><xsl:value-of select="//h:code[@code='1020000003']/../h:value" /></td></tr>
	  <tr><th class="first">初回面接の実施日付</th><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3003']/../h:effectiveTime/@value" /></td></tr>
	  <tr>
	    <th class="first">初回面接の支援形態</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3003']/@code = 1">(1) 個別支援</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3003']/@code = 2">(2) グループ支援</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3003']/@code = 3">(3) 電話</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3003']/@code = 4">(4) e-mail</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="first">初回面接実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1022000013']/..//h:width/@value" />
	      <xsl:value-of select="//h:code[@code='1022000013']/..//h:width/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">初回面接の実施者</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='90030']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 1">(1) 医師</xsl:when>
		<xsl:when test="//h:code[@code='90030']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 2">(2) 保健師</xsl:when>
		<xsl:when test="//h:code[@code='90030']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 3">(3) 管理栄養士</xsl:when>
		<xsl:when test="//h:code[@code='90030']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 4">(4) その他</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="first">継続的支援予定期間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021000020']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021000020']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">目標腹囲</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">目標体重</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">一日の削減目標エネルギー量</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001050']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001050']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">一日の運動による目標エネルギー量</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001051']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001051']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="first">一日の食事による目標エネルギー量</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001052']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001052']/../h:value/@unit" />
	    </td>
	  </tr>

	  <tr><th class="plan">計画上の継続的な支援の実施回数</th><td><xsl:value-of select="//h:code[@code='1041800117']/../h:value/@value" />回</td></tr>
	  <tr><th class="plan">計画上の個別支援A回数</th><td><xsl:value-of select="//h:code[@code='1041101117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="plan">計画上の個別支援A合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041101113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041101113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="plan">計画上のグループ支援回数</th><td><xsl:value-of select="//h:code[@code='1041302117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="plan">計画上のグループ支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041302113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041302113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="plan">計画上の電話A支援回数</th><td><xsl:value-of select="//h:code[@code='1041103117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="plan">計画上の電話A支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041103113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041103113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="plan">計画上のe-mailA支援回数</th><td><xsl:value-of select="//h:code[@code='1041104117']/../h:value/@value"/>回</td></tr>
	  <tr><th class="plan">計画上の支援Aポイント</th><td><xsl:value-of select="//h:code[@code='1041100114']/../h:value/@value" /></td></tr>
	  <tr><th class="plan">計画上の個別支援B回数</th><td><xsl:value-of select="//h:code[@code='1041201117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="plan">計画上の個別支援B合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041201113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041201113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="plan">計画上の電話B支援回数</th><td><xsl:value-of select="//h:code[@code='1041203117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="plan">計画上の電話B支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041203113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041203113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="plan">計画上のe-mailB支援回数</th><td><xsl:value-of select="//h:code[@code='1041204117']/../h:value/@value"/>回</td></tr>
	  <tr><th class="plan">計画上の支援Bポイント</th><td><xsl:value-of select="//h:code[@code='1041200114']/../h:value/@value" /></td></tr>
	  <tr><th class="plan">計画上の支援ポイント合計</th><td><xsl:value-of select="//h:code[@code='1041800114']/../h:value/@value" /></td></tr>

	  <tr><th class="inter">中間評価日時</th><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3004']/../h:effectiveTime/@value" /></td></tr>
	  <tr>
	    <th class="inter">中間評価の支援形態または確認方法</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/@code = 1">(1) 個別支援A</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/@code = 3">(3) グループ支援</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/@code = 4">(4) 電話A</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/@code = 6">(6) 電子メール支援A</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032000013']/../h:effectiveTime/h:width/@value" />
	      <xsl:value-of select="//h:code[@code='1032000013']/../h:effectiveTime/h:width/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価実施ポイント</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032000014']/../h:value/@value" />pt
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価の実施者</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/..//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 1">(1) 医師</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/..//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 2">(2) 保健師</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/..//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 3">(3) 管理栄養士</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3004']/..//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 4">(4) その他</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の腹囲</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の体重</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の収縮期血圧</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001033']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001033']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の拡張期血圧</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001034']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001034']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の生活習慣の改善(栄養・食生活)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1032001042']/../h:value/@code = 0">(0) 変化なし</xsl:when>
		<xsl:when test="//h:code[@code='1032001042']/../h:value/@code = 1">(1) 改善</xsl:when>
		<xsl:when test="//h:code[@code='1032001042']/../h:value/@code = 2">(2) 悪化</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の生活習慣の改善(身体活動)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1032001041']/../h:value/@code = 0">(0) 変化なし</xsl:when>
		<xsl:when test="//h:code[@code='1032001041']/../h:value/@code = 1">(1) 改善</xsl:when>
		<xsl:when test="//h:code[@code='1032001041']/../h:value/@code = 2">(2) 悪化</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="inter">中間評価時の生活習慣の改善(喫煙)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1032001043']/../h:value/@code = 1">(1) 禁煙継続</xsl:when>
		<xsl:when test="//h:code[@code='1032001043']/../h:value/@code = 2">(2) 禁煙非継続</xsl:when>
		<xsl:when test="//h:code[@code='1032001043']/../h:value/@code = 3">(3) 非喫煙</xsl:when>
		<xsl:when test="//h:code[@code='1032001043']/../h:value/@code = 4">(4) 禁煙意志なし</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>

	  <tr><th class="final">最終評価日時</th><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3005']/../h:effectiveTime/@value" /></td></tr>
	  <tr>
	    <th class="final">最終評価の支援形態</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3005']/@code = 1">(1) 個別支援</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3005']/@code = 2">(2) グループ支援</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3005']/@code = 3">(3) 電話</xsl:when>
		<xsl:when test="//h:code[@codeSystem='1.2.392.200119.6.3005']/@code = 4">(4) e-mail</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価の実施者</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='90060']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 1">(1) 医師</xsl:when>
		<xsl:when test="//h:code[@code='90060']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 2">(2) 保健師</xsl:when>
		<xsl:when test="//h:code[@code='90060']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 3">(3) 管理栄養士</xsl:when>
		<xsl:when test="//h:code[@code='90060']/../h:entry//h:code[@codeSystem='1.2.392.200119.6.3020']/@code = 4">(4) その他</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>	  
	  <tr><th class="final">最終評価できない場合の確認回数</th><td><xsl:value-of select="//h:code[@code='1042000116']/../h:value/@value" />回</td></tr>
	  <tr>
	    <th class="final">最終評価時の腹囲</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の体重</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の収縮期血圧</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001033']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001033']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の拡張期血圧</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001034']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001034']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の生活習慣の改善(栄養・食生活)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1042001042']/../h:value/@code = 0">(0) 変化なし</xsl:when>
		<xsl:when test="//h:code[@code='1042001042']/../h:value/@code = 1">(1) 改善</xsl:when>
		<xsl:when test="//h:code[@code='1042001042']/../h:value/@code = 2">(2) 悪化</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の生活習慣の改善(身体活動)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1042001041']/../h:value/@code = 0">(0) 変化なし</xsl:when>
		<xsl:when test="//h:code[@code='1042001041']/../h:value/@code = 1">(1) 改善</xsl:when>
		<xsl:when test="//h:code[@code='1042001041']/../h:value/@code = 2">(2) 悪化</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr>
	    <th class="final">最終評価時の生活習慣の改善(喫煙)</th>
	    <td>
	      <xsl:choose>
		<xsl:when test="//h:code[@code='1042001043']/../h:value/@code = 1">(1) 禁煙継続</xsl:when>
		<xsl:when test="//h:code[@code='1042001043']/../h:value/@code = 2">(2) 禁煙非継続</xsl:when>
		<xsl:when test="//h:code[@code='1042001043']/../h:value/@code = 3">(3) 非喫煙</xsl:when>
		<xsl:when test="//h:code[@code='1042001043']/../h:value/@code = 4">(4) 禁煙意志なし</xsl:when>
	      </xsl:choose>
	    </td>
	  </tr>
	  <tr><th class="result">実施上の継続的な支援の実施回数</th><td><xsl:value-of select="//h:code[@code='1042800117']/../h:value/@value" />回</td></tr>
	  <tr><th class="result">実施上の個別支援A回数</th><td><xsl:value-of select="//h:code[@code='1042101117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="result">実施上の個別支援A合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042101113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042101113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="result">実施上のグループ支援回数</th><td><xsl:value-of select="//h:code[@code='1042302117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="result">実施上のグループ支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042302113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042302113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="result">実施上の電話A支援回数</th><td><xsl:value-of select="//h:code[@code='1042103117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="result">実施上の電話A支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042103113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042103113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="result">実施上のe-mailA支援回数</th><td><xsl:value-of select="//h:code[@code='1042104117']/../h:value/@value"/>回</td></tr>
	  <tr><th class="result">実施上の支援Aポイント</th><td><xsl:value-of select="//h:code[@code='1042100114']/../h:value/@value" /></td></tr>
	  <tr><th class="result">実施上の個別支援B回数</th><td><xsl:value-of select="//h:code[@code='1042201117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="result">実施上の個別支援B合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042201113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042201113']/../h:value/@unit"/>
	    </td>
	  </tr>

	  <tr><th class="result">実施上の電話B支援回数</th><td><xsl:value-of select="//h:code[@code='1042203117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <th class="result">実施上の電話B支援合計実施時間</th>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042203113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042203113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><th class="result">実施上のe-mailB支援回数</th><td><xsl:value-of select="//h:code[@code='1042204117']/../h:value/@value"/>回</td></tr>
	  <tr><th class="result">実施上の支援Bポイント</th><td><xsl:value-of select="//h:code[@code='1042200114']/../h:value/@value" /></td></tr>
	  <tr><th class="result">実施上の支援ポイント合計</th><td><xsl:value-of select="//h:code[@code='1042800114']/../h:value/@value" /></td></tr>
	  <tr><th class="final">禁煙の指導回数</th><td><xsl:value-of select="//h:code[@code='1042800118']/../h:value/@value" />回</td></tr>
	  <tr><th class="result">実施上の継続的支援終了日</th><td><xsl:value-of select="//h:code[@code='1042000022']/../h:value" /></td></tr>
	</table>
	<br/>
	<br/>
	<table class="continual">
	  <tr>
	    <th>支援実施形態</th>
	    <th>支援実施日付</th>
	    <th>支援実施者</th>
	    <th>支援A時間</th>
	    <th>支援Apt</th>
	    <th>支援B時間</th>
	    <th>支援Bpt</th>
	  </tr>
	  <xsl:for-each select="//h:code[@code='90040']/../h:entry">
	    <tr>
	      <td>
		<xsl:choose>
		  <xsl:when test="current()//h:code/@code = 1">(1) 個別支援A</xsl:when>
		  <xsl:when test="current()//h:code/@code = 2">(2) 個別支援B</xsl:when>
		  <xsl:when test="current()//h:code/@code = 3">(3) グループ支援</xsl:when>
		  <xsl:when test="current()//h:code/@code = 4">(4) 電話A</xsl:when>
		  <xsl:when test="current()//h:code/@code = 5">(5) 電話B</xsl:when>
		  <xsl:when test="current()//h:code/@code = 6">(6) e-mailA</xsl:when>
		  <xsl:when test="current()//h:code/@code = 7">(7) e-mailB</xsl:when>
		</xsl:choose>
	      </td>
	      <td><xsl:value-of select="current()//h:act//h:effectiveTime/@value" /></td>
	      <td>
		<xsl:choose>
		  <xsl:when test="current()//h:performer//h:code/@code = 1">(1) 医師</xsl:when>
		  <xsl:when test="current()//h:performer//h:code/@code = 2">(2) 保健師</xsl:when>
		  <xsl:when test="current()//h:performer//h:code/@code = 3">(3) 管理栄養士</xsl:when>
		  <xsl:when test="current()//h:performer//h:code/@code = 4">(4) その他</xsl:when>
		</xsl:choose>
	      </td>
	      <td>
	      	<xsl:value-of select="current()//h:code[@code='1032100013']/../h:effectiveTime/h:width/@value" />
	      	<xsl:value-of select="current()//h:code[@code='1032100013']/../h:effectiveTime/h:width/@unit" />
	      </td>
	      <td><xsl:value-of select="current()//h:code[@code='1032100014']/../h:value/@value" />pt</td>
	      <td>
	      	<xsl:value-of select="current()//h:code[@code='1032200013']/../h:effectiveTime/h:width/@value" />
	      	<xsl:value-of select="current()//h:code[@code='1032200013']/../h:effectiveTime/h:width/@unit" />
	      </td>
	      <td><xsl:value-of select="current()//h:code[@code='1032200014']/../h:value/@value" />pt</td>
	    </tr>
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
