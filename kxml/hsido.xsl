<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet type="text/xml"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
		xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:h="urn:hl7-org:v3" xsi:schemaLocation="urn:hl7-org:v3 ../XSD/hg08_V08.xsd">
  <!-- <xsl:output method="html" encoding="UTF-8"/> -->
  <xsl:template match="/">
    <html>
      <body>
	<!-- <xsl:apply-templates select="//h:patientRole/h:patient"/> -->
	<table border="1">
	  <tr><td>氏名</td><td><xsl:value-of select="//h:patientRole//h:patient/h:name" /></td></tr>
	  <tr><td>記号</td><td><xsl:value-of select="//h:patientRole//h:id[@root='1.2.392.200119.6.204']/@extension" /></td></tr>
	  <tr><td>番号</td><td><xsl:value-of select="//h:patientRole//h:id[@root='1.2.392.200119.6.205']/@extension" /></td></tr>
	  <tr><td>生年月日</td><td><xsl:value-of select="//h:patientRole//h:birthTime/@value" /></td></tr>
	  <tr><td>医療機関CD</td><td><xsl:value-of select="//h:representedOrganization/h:id/@extension" /></td></tr>
	  <tr><td>医療機関</td><td><xsl:value-of select="//h:representedOrganization//h:name" /></td></tr>
	  <tr><td>受診券番号</td><td><xsl:value-of select="//h:id[@root='1.2.392.200119.6.209.100263129']/@extension" /></td></tr>
	  <tr><td>利用券番号</td><td><xsl:value-of select="//h:id[@root='1.2.392.200119.6.210.100263129']/@extension" /></td></tr>
	  <tr><td>実施日</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.1002']/../h:effectiveTime/@value" /></td></tr>
	  <tr><td>支援レベル</td><td><xsl:value-of select="//h:code[@code='1020000001']/../h:value/@code" /></td></tr>
	  <tr><td>行動変容ステージ</td><td><xsl:value-of select="//h:code[@code='1020000002']/../h:value/@code" /></td></tr>
	  <tr><td>指導コース名</td><td><xsl:value-of select="//h:code[@code='1020000003']/../h:value" /></td></tr>

	  <tr><td>初回面接の支援形態</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3003']/@code" /></td></tr>
	  <tr><td>初回面接の実施日付</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3003']/../h:effectiveTime/@value" /></td></tr>
	  <tr><td>初回面接の実施者</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3020']/@code" /></td></tr>
	  <tr>
	    <td>継続的支援予定機関</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021000020']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021000020']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>目標体重</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>目標腹囲</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>一日の削減目標エネルギー量</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001050']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001050']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>一日の運動による目標エネルギー量</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001051']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001051']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>一日の食事による目標エネルギー量</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1021001052']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1021001052']/../h:value/@unit" />
	    </td>
	  </tr>

	  <tr><td>計画上の継続的な支援の実施回数</td><td><xsl:value-of select="//h:code[@code='1041800117']/../h:value/@value" />回</td></tr>
	  <tr><td>計画上の個別支援A回数</td><td><xsl:value-of select="//h:code[@code='1041101117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>計画上の個別支援A合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041101113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041101113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>計画上の個別支援B回数</td><td><xsl:value-of select="//h:code[@code='1041201117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>計画上の個別支援B合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041201113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041201113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>計画上のグループ支援回数</td><td><xsl:value-of select="//h:code[@code='1041302117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>計画上のグループ支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041302113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041302113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>計画上の電話A支援回数</td><td><xsl:value-of select="//h:code[@code='1041103117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>計画上の電話A支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041103113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041103113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>計画上のe-mailA支援回数</td><td><xsl:value-of select="//h:code[@code='1041104117']/../h:value/@value"/>回</td></tr>
	  <tr><td>計画上の電話B支援回数</td><td><xsl:value-of select="//h:code[@code='1041203117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>計画上の電話B支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1041203113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1041203113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>計画上のe-mailB支援回数</td><td><xsl:value-of select="//h:code[@code='1041204117']/../h:value/@value"/>回</td></tr>
	  <tr><td>計画上の支援Aポイント</td><td><xsl:value-of select="//h:code[@code='1041100114']/../h:value/@value" /></td></tr>
	  <tr><td>計画上の支援Bポイント</td><td><xsl:value-of select="//h:code[@code='1041200114']/../h:value/@value" /></td></tr>
	  <tr><td>計画上の支援ポイント合計</td><td><xsl:value-of select="//h:code[@code='1041800114']/../h:value/@value" /></td></tr>

	  <tr><td>実施上の継続的な支援の実施回数</td><td><xsl:value-of select="//h:code[@code='1042800117']/../h:value/@value" />回</td></tr>
	  <tr><td>実施上の個別支援A回数</td><td><xsl:value-of select="//h:code[@code='1042101117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>実施上の個別支援A合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042101113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042101113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>実施上の個別支援B回数</td><td><xsl:value-of select="//h:code[@code='1042201117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>実施上の個別支援B合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042201113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042201113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>実施上のグループ支援回数</td><td><xsl:value-of select="//h:code[@code='1042302117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>実施上のグループ支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042302113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042302113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>実施上の電話A支援回数</td><td><xsl:value-of select="//h:code[@code='1042103117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>実施上の電話A支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042103113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042103113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>実施上のe-mailA支援回数</td><td><xsl:value-of select="//h:code[@code='1042104117']/../h:value/@value"/>回</td></tr>
	  <tr><td>実施上の電話B支援回数</td><td><xsl:value-of select="//h:code[@code='1042203117']/../h:value/@value"/>回</td></tr>
	  <tr>
	    <td>実施上の電話B支援合計実施時間</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042203113']/../h:value/@value"/>
	      <xsl:value-of select="//h:code[@code='1042203113']/../h:value/@unit"/>
	    </td>
	  </tr>
	  <tr><td>実施上のe-mailB支援回数</td><td><xsl:value-of select="//h:code[@code='1042204117']/../h:value/@value"/>回</td></tr>
	  <tr><td>実施上の支援Aポイント</td><td><xsl:value-of select="//h:code[@code='1042100114']/../h:value/@value" /></td></tr>
	  <tr><td>実施上の支援Bポイント</td><td><xsl:value-of select="//h:code[@code='1042200114']/../h:value/@value" /></td></tr>
	  <tr><td>実施上の支援ポイント合計</td><td><xsl:value-of select="//h:code[@code='1042800114']/../h:value/@value" /></td></tr>
	  <tr><td>実施上の継続的支援終了日</td><td><xsl:value-of select="//h:code[@code='1042000022']/../h:value" /></td></tr>
	  <tr><td>中間評価日時</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3004']/../h:effectiveTime/@value" /></td></tr>
	  <tr><td>中間評価の支援形態または確認方法</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3004']/@code" /></td></tr>
	  <tr><td>中間評価の実施者</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3004']/..//h:code[@codeSystem='1.2.392.200119.6.3020']/@code" /></td></tr>
	  <tr>
	    <td>中間評価時の腹囲</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>中間評価時の体重</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>中間評価時の収縮期血圧</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001033']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001033']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>中間評価時の拡張期血圧</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1032001034']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1032001034']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr><td>中間評価時の生活習慣の改善(栄養・食生活)</td><td><xsl:value-of select="//h:code[@code='1032001042']/../h:value/@code" /></td></tr>
	  <tr><td>中間評価時の生活習慣の改善(身体活動)</td><td><xsl:value-of select="//h:code[@code='1032001041']/../h:value/@code" /></td></tr>
	  <tr><td>中間評価時の生活習慣の改善(喫煙)</td><td><xsl:value-of select="//h:code[@code='1032001043']/../h:value/@code" /></td></tr>



	  <tr><td>最終評価日時</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3005']/../h:effectiveTime/@value" /></td></tr>
	  <tr><td>最終評価の支援形態または確認方法</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3005']/@code" /></td></tr>
	  <tr><td>最終評価の実施者</td><td><xsl:value-of select="//h:code[@codeSystem='1.2.392.200119.6.3020']/@code" /></td></tr>
	  <tr>
	    <td>最終評価時の腹囲</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001031']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001031']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>最終評価時の体重</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001032']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001032']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>最終評価時の収縮期血圧</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001033']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001033']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr>
	    <td>最終評価時の拡張期血圧</td>
	    <td>
	      <xsl:value-of select="//h:code[@code='1042001034']/../h:value/@value" />
	      <xsl:value-of select="//h:code[@code='1042001034']/../h:value/@unit" />
	    </td>
	  </tr>
	  <tr><td>最終評価時の生活習慣の改善(栄養・食生活)</td><td><xsl:value-of select="//h:code[@code='1042001042']/../h:value/@code" /></td></tr>
	  <tr><td>最終評価時の生活習慣の改善(身体活動)</td><td><xsl:value-of select="//h:code[@code='1042001041']/../h:value/@code" /></td></tr>
	  <tr><td>最終評価時の生活習慣の改善(喫煙)</td><td><xsl:value-of select="//h:code[@code='1042001043']/../h:value/@code" /></td></tr>
	  <tr><td>禁煙の指導回数</td><td><xsl:value-of select="//h:code[@code='1042800118']/../h:value/@value" />回</td></tr>
	  <tr><td>最終評価できない場合の確認回数</td><td><xsl:value-of select="//h:code[@code='1042000116']/../h:value/@value" />回</td></tr>
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	  <!-- <tr><td></td><td><xsl:value-of select="" /></td></tr> -->
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
