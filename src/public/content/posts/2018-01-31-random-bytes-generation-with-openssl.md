Sometimes you just have to generate some random bytes with high entropy. And OpenSSL can help you with this task.

<!--more-->

``` bash
$ openssl rand [options] num
```

The `rand` command outputs `num` pseudo-random bytes. Since in most cases we don't want to deal with edge bytes, it's better to use either `-hex` or `-base64` option in order to encode output and thus lead to a valid password.

``` bash
$ openssl rand 16
fӤ?v ???^@?y??

$ openssl rand -hex 16
b4ef65a47a327727bf4ad77d8d3352b2

$ openssl rand -base64 16
o1DqThmx1DWGoPAidi6DKQ==
```

Another problem is the length. When you use `-hex` or `-base64` option the output string is longer than passed `num`. In order to keep the desired length, you have to chop it. For example, using the `cut` command.

``` bash
$ openssl rand -hex 16 | cut -c1-16
ce8ad63b50cbe611

$ openssl rand -base64 16 | cut -c1-16
4HIcmt4vTcmchbHU
```

Some useful links:

- [Rand command manual](https://wiki.openssl.org/index.php/Manual:Rand(1))
- [OpenSSL random numbers](https://wiki.openssl.org/index.php/Random_Numbers)
- [Password Haystack](https://www.grc.com/haystack.htm)
