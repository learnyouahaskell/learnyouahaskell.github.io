function ShowAndHide(name) {
    var x = document.getElementById(name);
    var z = document.getElementById("newsplash");
    var faq = document.getElementById("faq-button");
    var book = document.getElementById("book-button");
    var read = document.getElementById("read-button");
    if (x.style.display == 'none') {
        x.style.display = 'block';
        z.style = "width:880px;height:800px;margin:0px auto 0 auto;background-repeat:no-repeat;background-image:url(assets/images/newsplash-new-long.webp);position:relative;font-size:16px;color:white;text-shadow:#24536F -1px 1px 1px";
        faq.style = "display:block;text-indent:-9999px;position:absolute;width:265px;height:100px;top:720px;left:110px;";
        book.style = "display:block;text-indent:-9999px;position:absolute;width:167px;height:143px;top:662px;left:434px;";
        read.style = "display:block;text-indent:-9999px;position:absolute;width:167px;height:143px;top:662px;left:638px;";
    } else {
        x.style.display = 'none';
        z.style = "width:880px;height:800px;margin:0px auto 0 auto;background-repeat:no-repeat;background-image:url(assets/images/newsplash-new-short.webp);position:relative;font-size:16px;color:white;text-shadow:#24536F -1px 1px 1px";
        faq.style = "display:block;text-indent:-9999px;position:absolute;width:265px;height:100px;top:535px;left:110px;";
        book.style = "display:block;text-indent:-9999px;position:absolute;width:167px;height:143px;top:477px;left:434px;";
        read.style = "display:block;text-indent:-9999px;position:absolute;width:167px;height:143px;top:477px;left:638px;";
    }
}