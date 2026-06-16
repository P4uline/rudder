module Activity.HtmlStringAdapter exposing (htmlToString, html2html, mockHtml2Html, text)

import Html exposing (Html, text)
import Html.String as HtmlString exposing (a)
import Html.String.Attributes as HtmlStringAttributes exposing (href)

type alias HtmlDescription msg = HtmlString.Html msg


-- TODO delete
mockHtmlToString: String
mockHtmlToString = HtmlString.toString 0 (someHtml)

htmlToString : HtmlString.Html msg -> String
htmlToString html =
    HtmlString.toString 0 (html)

mockHtml2Html : Html msg
mockHtml2Html = HtmlString.toHtml someHtml

html2html : HtmlString.Html msg -> Html msg
html2html html = HtmlString.toHtml html


text : String -> HtmlString.Html msg
text s = HtmlString.text s

someHtml : HtmlString.Html msg
someHtml =
            HtmlString.a [ HtmlStringAttributes.href "http://google.com" ] [ HtmlString.text "Google!" ]
{-
htmlString2htmlLang : Html.Html msg
htmlString2htmlLang = HtmlString.toHtml someHtml

string2Html : String -> HtmlString.Html msg
string2Html s = someHtml-}
