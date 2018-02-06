# 2b7349ef54d940c8ae52cec4a74ec054cbfc31278e1fadbfdd2c4188aec21c90  package.json
{ fetchgit, fetchurl, globalBuildInputs ? [], nodeEnv, src }:
let
    sources = {
      "@ava/babel-plugin-throws-helper-2.0.0" = {
        name = "_at_ava_slash_babel-plugin-throws-helper";
        packageName = "@ava/babel-plugin-throws-helper";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/@ava/babel-plugin-throws-helper/-/babel-plugin-throws-helper-2.0.0.tgz";
          sha1 = "2fc1fe3c211a71071a4eca7b8f7af5842cd1ae7c";
        };
      };
      "@ava/babel-preset-stage-4-1.1.0" = {
        name = "_at_ava_slash_babel-preset-stage-4";
        packageName = "@ava/babel-preset-stage-4";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/@ava/babel-preset-stage-4/-/babel-preset-stage-4-1.1.0.tgz";
          sha512 = "0b5jqr4281xzd2ai3jnmygyj5hblw1xpjh5f7z1yzc42wgx2649w49lggi4bl1gv5sdqfv7p0xnbqyhsry7aqj1v4xpjnwph6f96sm1";
        };
      };
      "@ava/babel-preset-transform-test-files-3.0.0" = {
        name = "_at_ava_slash_babel-preset-transform-test-files";
        packageName = "@ava/babel-preset-transform-test-files";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/@ava/babel-preset-transform-test-files/-/babel-preset-transform-test-files-3.0.0.tgz";
          sha1 = "cded1196a8d8d9381a509240ab92e91a5ec069f7";
        };
      };
      "@ava/write-file-atomic-2.2.0" = {
        name = "_at_ava_slash_write-file-atomic";
        packageName = "@ava/write-file-atomic";
        version = "2.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/@ava/write-file-atomic/-/write-file-atomic-2.2.0.tgz";
          sha512 = "06ffajbqiakvbg1405j3r4jd9r15c6bq7c69hv0pab1hrk8rb1a2cc0izd313hivdqjivkbnlgx1xmgqnjvlw5gzd9z44cvf7g42cq5";
        };
      };
      "@concordance/react-1.0.0" = {
        name = "_at_concordance_slash_react";
        packageName = "@concordance/react";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/@concordance/react/-/react-1.0.0.tgz";
          sha512 = "36viswaaq6i7bs2a1d786f6p3y3jqyzwz1vjgq5avv2z90a5c186l5j7i03q4lrhma9wh0krg0krlgf2hsg7967jsqqiw0pli2yrnl6";
        };
      };
      "abab-1.0.4" = {
        name = "abab";
        packageName = "abab";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/abab/-/abab-1.0.4.tgz";
          sha1 = "5faad9c2c07f60dd76770f71cf025b62a63cfd4e";
        };
      };
      "accepts-1.3.5" = {
        name = "accepts";
        packageName = "accepts";
        version = "1.3.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/accepts/-/accepts-1.3.5.tgz";
          sha1 = "eb777df6011723a3b14e8a72c0805c8e86746bd2";
        };
      };
      "acorn-4.0.13" = {
        name = "acorn";
        packageName = "acorn";
        version = "4.0.13";
        src = fetchurl {
          url = "https://registry.npmjs.org/acorn/-/acorn-4.0.13.tgz";
          sha1 = "105495ae5361d697bd195c825192e1ad7f253787";
        };
      };
      "acorn-5.5.3" = {
        name = "acorn";
        packageName = "acorn";
        version = "5.5.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/acorn/-/acorn-5.5.3.tgz";
          sha512 = "0wmwifv9mm9gqcir9zbz5y1gl1rgwwprqh1f3csjydj8kf3byca7img3rh5b54kbnw3ik34bc6ynbnzsd01zmxrsfdvjv95hn84rpld";
        };
      };
      "acorn-dynamic-import-2.0.2" = {
        name = "acorn-dynamic-import";
        packageName = "acorn-dynamic-import";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/acorn-dynamic-import/-/acorn-dynamic-import-2.0.2.tgz";
          sha1 = "c752bd210bef679501b6c6cb7fc84f8f47158cc4";
        };
      };
      "acorn-globals-4.1.0" = {
        name = "acorn-globals";
        packageName = "acorn-globals";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/acorn-globals/-/acorn-globals-4.1.0.tgz";
          sha512 = "0wi1iz0xrr4nh1gj6191z11fvwsfjzwv7hidz5d7n0nhljviqm24jrr9z395kyi586yi9gsijr5qc9gsjs1j7wpg6x7n6xfdr9p0dia";
        };
      };
      "after-0.8.2" = {
        name = "after";
        packageName = "after";
        version = "0.8.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/after/-/after-0.8.2.tgz";
          sha1 = "fedb394f9f0e02aa9768e702bda23b505fae7e1f";
        };
      };
      "ajv-5.5.2" = {
        name = "ajv";
        packageName = "ajv";
        version = "5.5.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/ajv/-/ajv-5.5.2.tgz";
          sha1 = "73b5eeca3fab653e3d3f9422b341ad42205dc965";
        };
      };
      "ajv-6.2.1" = {
        name = "ajv";
        packageName = "ajv";
        version = "6.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ajv/-/ajv-6.2.1.tgz";
          sha1 = "28a6abc493a2abe0fb4c8507acaedb43fa550671";
        };
      };
      "ajv-keywords-3.1.0" = {
        name = "ajv-keywords";
        packageName = "ajv-keywords";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ajv-keywords/-/ajv-keywords-3.1.0.tgz";
          sha1 = "ac2b27939c543e95d2c06e7f7f5c27be4aa543be";
        };
      };
      "align-text-0.1.4" = {
        name = "align-text";
        packageName = "align-text";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/align-text/-/align-text-0.1.4.tgz";
          sha1 = "0cd90a561093f35d0a99256c22b7069433fad117";
        };
      };
      "alphanum-sort-1.0.2" = {
        name = "alphanum-sort";
        packageName = "alphanum-sort";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/alphanum-sort/-/alphanum-sort-1.0.2.tgz";
          sha1 = "97a1119649b211ad33691d9f9f486a8ec9fbe0a3";
        };
      };
      "animate.css-3.6.1" = {
        name = "animate.css";
        packageName = "animate.css";
        version = "3.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/animate.css/-/animate.css-3.6.1.tgz";
          sha512 = "3n04ajc1bqc5gji0fz28dhj204hi0c3h0a9lm8npli04b3v24d758skhw78saqlxcdm6mhb8h06vapyklnv8ayqjn409gchmk636hn4";
        };
      };
      "ansi-align-2.0.0" = {
        name = "ansi-align";
        packageName = "ansi-align";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-align/-/ansi-align-2.0.0.tgz";
          sha1 = "c36aeccba563b89ceb556f3690f0b1d9e3547f7f";
        };
      };
      "ansi-escapes-2.0.0" = {
        name = "ansi-escapes";
        packageName = "ansi-escapes";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-2.0.0.tgz";
          sha1 = "5bae52be424878dd9783e8910e3fc2922e83c81b";
        };
      };
      "ansi-html-0.0.7" = {
        name = "ansi-html";
        packageName = "ansi-html";
        version = "0.0.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-html/-/ansi-html-0.0.7.tgz";
          sha1 = "813584021962a9e9e6fd039f940d12f56ca7859e";
        };
      };
      "ansi-regex-2.1.1" = {
        name = "ansi-regex";
        packageName = "ansi-regex";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-2.1.1.tgz";
          sha1 = "c3b33ab5ee360d86e0e628f0468ae7ef27d654df";
        };
      };
      "ansi-regex-3.0.0" = {
        name = "ansi-regex";
        packageName = "ansi-regex";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-3.0.0.tgz";
          sha1 = "ed0317c322064f79466c02966bddb605ab37d998";
        };
      };
      "ansi-styles-1.0.0" = {
        name = "ansi-styles";
        packageName = "ansi-styles";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-1.0.0.tgz";
          sha1 = "cb102df1c56f5123eab8b67cd7b98027a0279178";
        };
      };
      "ansi-styles-2.2.1" = {
        name = "ansi-styles";
        packageName = "ansi-styles";
        version = "2.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-2.2.1.tgz";
          sha1 = "b432dd3358b634cf75e1e4664368240533c1ddbe";
        };
      };
      "ansi-styles-3.2.1" = {
        name = "ansi-styles";
        packageName = "ansi-styles";
        version = "3.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz";
          sha512 = "2lgkskkj4c1fsnrksy0yffda0wss84p8lfiazdc7jli7iqnvrxkzbxjzpvx13lm28qw0zkawfxvz2bdiisc72ccy7hx8i8rm4iijgam";
        };
      };
      "anymatch-1.3.2" = {
        name = "anymatch";
        packageName = "anymatch";
        version = "1.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/anymatch/-/anymatch-1.3.2.tgz";
          sha512 = "269dbx666z4ws49vag1dma5kdpjlx83s74c1jlngrn2672rhvbc47i5ay5h40spmrzgvbvcm33i4yrp88rrc6lg70v78k155z45lwyi";
        };
      };
      "anymatch-2.0.0" = {
        name = "anymatch";
        packageName = "anymatch";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/anymatch/-/anymatch-2.0.0.tgz";
          sha512 = "03mjsaw6xk4zhvl17fpqn59j4v2bafqs0yfw5y45hl8x97xlihwvjmcx3icnaamvipplnczymvzg4sb4ixwpzak0k3p21c00nqqxmz6";
        };
      };
      "aproba-1.2.0" = {
        name = "aproba";
        packageName = "aproba";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/aproba/-/aproba-1.2.0.tgz";
          sha512 = "13mgnbmdhdq0qncijvpip1l39q1a8labcvj3hc3n1yl2zch106mdkn7p7bd5knvmfkkn1js9nd47nzyjk1himbm8ry8i8gd6mk7mlk3";
        };
      };
      "argparse-1.0.10" = {
        name = "argparse";
        packageName = "argparse";
        version = "1.0.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz";
          sha512 = "17fhywmdjcp8g0ys089i5xg26yajkkg9zg8hyry3fghz75b2pq5vdx997dk5p2sc15r9nsj8rmf4f27g81hi92kzj5q86sdmg5ni553";
        };
      };
      "arr-diff-2.0.0" = {
        name = "arr-diff";
        packageName = "arr-diff";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/arr-diff/-/arr-diff-2.0.0.tgz";
          sha1 = "8f3b827f955a8bd669697e4a4256ac3ceae356cf";
        };
      };
      "arr-diff-4.0.0" = {
        name = "arr-diff";
        packageName = "arr-diff";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/arr-diff/-/arr-diff-4.0.0.tgz";
          sha1 = "d6461074febfec71e7e15235761a329a5dc7c520";
        };
      };
      "arr-exclude-1.0.0" = {
        name = "arr-exclude";
        packageName = "arr-exclude";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/arr-exclude/-/arr-exclude-1.0.0.tgz";
          sha1 = "dfc7c2e552a270723ccda04cf3128c8cbfe5c631";
        };
      };
      "arr-flatten-1.1.0" = {
        name = "arr-flatten";
        packageName = "arr-flatten";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/arr-flatten/-/arr-flatten-1.1.0.tgz";
          sha512 = "2vdly17xk5kw7bfzajrjdnw4ml3wrfblx8064n0i4fxlchcscx2mvnwkq2bnnqvbqvdy4vs9ad462lz0rid7khysly9m9vzjiblly1g";
        };
      };
      "arr-union-3.1.0" = {
        name = "arr-union";
        packageName = "arr-union";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/arr-union/-/arr-union-3.1.0.tgz";
          sha1 = "e39b09aea9def866a8f206e288af63919bae39c4";
        };
      };
      "array-differ-1.0.0" = {
        name = "array-differ";
        packageName = "array-differ";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-differ/-/array-differ-1.0.0.tgz";
          sha1 = "eff52e3758249d33be402b8bb8e564bb2b5d4031";
        };
      };
      "array-equal-1.0.0" = {
        name = "array-equal";
        packageName = "array-equal";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-equal/-/array-equal-1.0.0.tgz";
          sha1 = "8c2a5ef2472fd9ea742b04c77a75093ba2757c93";
        };
      };
      "array-find-index-1.0.2" = {
        name = "array-find-index";
        packageName = "array-find-index";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-find-index/-/array-find-index-1.0.2.tgz";
          sha1 = "df010aa1287e164bbda6f9723b0a96a1ec4187a1";
        };
      };
      "array-flatten-1.1.1" = {
        name = "array-flatten";
        packageName = "array-flatten";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-flatten/-/array-flatten-1.1.1.tgz";
          sha1 = "9a5f699051b1e7073328f2a008968b64ea2955d2";
        };
      };
      "array-flatten-2.1.1" = {
        name = "array-flatten";
        packageName = "array-flatten";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-flatten/-/array-flatten-2.1.1.tgz";
          sha1 = "426bb9da84090c1838d812c8150af20a8331e296";
        };
      };
      "array-includes-3.0.3" = {
        name = "array-includes";
        packageName = "array-includes";
        version = "3.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-includes/-/array-includes-3.0.3.tgz";
          sha1 = "184b48f62d92d7452bb31b323165c7f8bd02266d";
        };
      };
      "array-union-1.0.2" = {
        name = "array-union";
        packageName = "array-union";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-union/-/array-union-1.0.2.tgz";
          sha1 = "9a34410e4f4e3da23dea375be5be70f24778ec39";
        };
      };
      "array-uniq-1.0.3" = {
        name = "array-uniq";
        packageName = "array-uniq";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-uniq/-/array-uniq-1.0.3.tgz";
          sha1 = "af6ac877a25cc7f74e058894753858dfdb24fdb6";
        };
      };
      "array-unique-0.2.1" = {
        name = "array-unique";
        packageName = "array-unique";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-unique/-/array-unique-0.2.1.tgz";
          sha1 = "a1d97ccafcbc2625cc70fadceb36a50c58b01a53";
        };
      };
      "array-unique-0.3.2" = {
        name = "array-unique";
        packageName = "array-unique";
        version = "0.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/array-unique/-/array-unique-0.3.2.tgz";
          sha1 = "a894b75d4bc4f6cd679ef3244a9fd8f46ae2d428";
        };
      };
      "arraybuffer.slice-0.0.7" = {
        name = "arraybuffer.slice";
        packageName = "arraybuffer.slice";
        version = "0.0.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/arraybuffer.slice/-/arraybuffer.slice-0.0.7.tgz";
          sha512 = "2ifpj39fza01g4z9jhgl0shmh5f79czgfh7bf40n66v5p93nrf43kiqhsgic9az2jrwmj8n60dn7kav1rzvm41a9kwi4ypf0mahhrf0";
        };
      };
      "arrify-1.0.1" = {
        name = "arrify";
        packageName = "arrify";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/arrify/-/arrify-1.0.1.tgz";
          sha1 = "898508da2226f380df904728456849c1501a4b0d";
        };
      };
      "asap-2.0.6" = {
        name = "asap";
        packageName = "asap";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/asap/-/asap-2.0.6.tgz";
          sha1 = "e50347611d7e690943208bbdafebcbc2fb866d46";
        };
      };
      "asn1-0.2.3" = {
        name = "asn1";
        packageName = "asn1";
        version = "0.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/asn1/-/asn1-0.2.3.tgz";
          sha1 = "dac8787713c9966849fc8180777ebe9c1ddf3b86";
        };
      };
      "asn1.js-4.10.1" = {
        name = "asn1.js";
        packageName = "asn1.js";
        version = "4.10.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/asn1.js/-/asn1.js-4.10.1.tgz";
          sha512 = "15j9ms7k6dx08m1wqrvc4958vcrshjjpicpxmz9bflsyk0x2r7w20lxlm27jpyj7cwj6158a0582vifk393hqp0ysn9mlbabqw9qzd7";
        };
      };
      "assert-1.4.1" = {
        name = "assert";
        packageName = "assert";
        version = "1.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/assert/-/assert-1.4.1.tgz";
          sha1 = "99912d591836b5a6f5b345c0f07eefc08fc65d91";
        };
      };
      "assert-plus-1.0.0" = {
        name = "assert-plus";
        packageName = "assert-plus";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz";
          sha1 = "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525";
        };
      };
      "assign-symbols-1.0.0" = {
        name = "assign-symbols";
        packageName = "assign-symbols";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/assign-symbols/-/assign-symbols-1.0.0.tgz";
          sha1 = "59667f41fadd4f20ccbc2bb96b8d4f7f78ec0367";
        };
      };
      "async-1.5.2" = {
        name = "async";
        packageName = "async";
        version = "1.5.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/async/-/async-1.5.2.tgz";
          sha1 = "ec6a61ae56480c0c3cb241c95618e20892f9672a";
        };
      };
      "async-2.6.0" = {
        name = "async";
        packageName = "async";
        version = "2.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/async/-/async-2.6.0.tgz";
          sha512 = "0zp4b5788400npi1ixjry5x3a4m21c8pnknk8v731rgnwnjbp5ijmfcf5ppmn1ap4a04md1s9dr8n9ygdvrmiai590v0k6dby1wc1y4";
        };
      };
      "async-each-1.0.1" = {
        name = "async-each";
        packageName = "async-each";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/async-each/-/async-each-1.0.1.tgz";
          sha1 = "19d386a1d9edc6e7c1c85d388aedbcc56d33602d";
        };
      };
      "async-limiter-1.0.0" = {
        name = "async-limiter";
        packageName = "async-limiter";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/async-limiter/-/async-limiter-1.0.0.tgz";
          sha512 = "1ddib7nbyayhldvsyrfdpxk7khyi6s72570gkf3qqf4b1xwzdh52w0vlj6bknl40imispychhwfjb2bm29pjxbd5yz26fi8g8bfx7wf";
        };
      };
      "asynckit-0.4.0" = {
        name = "asynckit";
        packageName = "asynckit";
        version = "0.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz";
          sha1 = "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79";
        };
      };
      "atob-2.0.3" = {
        name = "atob";
        packageName = "atob";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/atob/-/atob-2.0.3.tgz";
          sha1 = "19c7a760473774468f20b2d2d03372ad7d4cbf5d";
        };
      };
      "auto-bind-1.2.0" = {
        name = "auto-bind";
        packageName = "auto-bind";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/auto-bind/-/auto-bind-1.2.0.tgz";
          sha512 = "0wamaj1k757h28fyrvfam4fz8ymz84pvfcyvm3k88bs8vxq36jn9kbiqqa3s0axwi6pcmwgmpjqfsh2721a1bb5kp5dpkpdkrkfj3k7";
        };
      };
      "autoprefixer-6.7.7" = {
        name = "autoprefixer";
        packageName = "autoprefixer";
        version = "6.7.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/autoprefixer/-/autoprefixer-6.7.7.tgz";
          sha1 = "1dbd1c835658e35ce3f9984099db00585c782014";
        };
      };
      "autoprefixer-7.2.6" = {
        name = "autoprefixer";
        packageName = "autoprefixer";
        version = "7.2.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/autoprefixer/-/autoprefixer-7.2.6.tgz";
          sha512 = "2lwdiknh34r4v5q9iwrffd3062w91lk2z4737yzzdyaqffmpgm0bk0iq8r9z4hx05c37apppxhmrpkjzg1qbimnya8dgzvyh1217br2";
        };
      };
      "ava-0.20.0" = {
        name = "ava";
        packageName = "ava";
        version = "0.20.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ava/-/ava-0.20.0.tgz";
          sha512 = "107646msp6jp1w5llpnnkddmj9wlv86433pi7pjcx2a4jzb6jfgz49n6nk0fh4ax60961899anybmrv9rm6wy301hrxb9k0017kz77w";
        };
      };
      "ava-0.23.0" = {
        name = "ava";
        packageName = "ava";
        version = "0.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ava/-/ava-0.23.0.tgz";
          sha512 = "1j3ra0gzd5msr8gb35sr61yswizs13sfw1n6vlyi8jdnmrnhvxh5k1izwxqb3mgd5m03w4r92wqd17adx03h42n3qckld04qlxp1ib6";
        };
      };
      "ava-init-0.2.1" = {
        name = "ava-init";
        packageName = "ava-init";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ava-init/-/ava-init-0.2.1.tgz";
          sha512 = "2b20bmhxk0nspcm6slwzn79v2da722fjddslvgf7irw8rwqnjcm3lry9whnnsmaik24wh9m6smyszs9kicmnsilp1g0vniyngj0lz4m";
        };
      };
      "aws-sign2-0.7.0" = {
        name = "aws-sign2";
        packageName = "aws-sign2";
        version = "0.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.7.0.tgz";
          sha1 = "b46e890934a9591f2d2f6f86d7e6a9f1b3fe76a8";
        };
      };
      "aws4-1.6.0" = {
        name = "aws4";
        packageName = "aws4";
        version = "1.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/aws4/-/aws4-1.6.0.tgz";
          sha1 = "83ef5ca860b2b32e4a0deedee8c771b9db57471e";
        };
      };
      "babel-code-frame-6.26.0" = {
        name = "babel-code-frame";
        packageName = "babel-code-frame";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-code-frame/-/babel-code-frame-6.26.0.tgz";
          sha1 = "63fd43f7dc1e3bb7ce35947db8fe369a3f58c74b";
        };
      };
      "babel-core-6.26.0" = {
        name = "babel-core";
        packageName = "babel-core";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-core/-/babel-core-6.26.0.tgz";
          sha1 = "af32f78b31a6fcef119c87b0fd8d9753f03a0bb8";
        };
      };
      "babel-generator-6.26.1" = {
        name = "babel-generator";
        packageName = "babel-generator";
        version = "6.26.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-generator/-/babel-generator-6.26.1.tgz";
          sha512 = "2jfjq7ggb0ygyzxg3vw248iq0iw2v00janjva1bf3l322k4zjcx9hvq9qqy67gxxyl2igw1asxfgp769hd5229pc70kwri9l1iz09qz";
        };
      };
      "babel-helper-bindify-decorators-6.24.1" = {
        name = "babel-helper-bindify-decorators";
        packageName = "babel-helper-bindify-decorators";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-bindify-decorators/-/babel-helper-bindify-decorators-6.24.1.tgz";
          sha1 = "14c19e5f142d7b47f19a52431e52b1ccbc40a330";
        };
      };
      "babel-helper-builder-binary-assignment-operator-visitor-6.24.1" = {
        name = "babel-helper-builder-binary-assignment-operator-visitor";
        packageName = "babel-helper-builder-binary-assignment-operator-visitor";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-builder-binary-assignment-operator-visitor/-/babel-helper-builder-binary-assignment-operator-visitor-6.24.1.tgz";
          sha1 = "cce4517ada356f4220bcae8a02c2b346f9a56664";
        };
      };
      "babel-helper-builder-react-jsx-6.26.0" = {
        name = "babel-helper-builder-react-jsx";
        packageName = "babel-helper-builder-react-jsx";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-builder-react-jsx/-/babel-helper-builder-react-jsx-6.26.0.tgz";
          sha1 = "39ff8313b75c8b65dceff1f31d383e0ff2a408a0";
        };
      };
      "babel-helper-call-delegate-6.24.1" = {
        name = "babel-helper-call-delegate";
        packageName = "babel-helper-call-delegate";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-call-delegate/-/babel-helper-call-delegate-6.24.1.tgz";
          sha1 = "ece6aacddc76e41c3461f88bfc575bd0daa2df8d";
        };
      };
      "babel-helper-define-map-6.26.0" = {
        name = "babel-helper-define-map";
        packageName = "babel-helper-define-map";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-define-map/-/babel-helper-define-map-6.26.0.tgz";
          sha1 = "a5f56dab41a25f97ecb498c7ebaca9819f95be5f";
        };
      };
      "babel-helper-explode-assignable-expression-6.24.1" = {
        name = "babel-helper-explode-assignable-expression";
        packageName = "babel-helper-explode-assignable-expression";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-explode-assignable-expression/-/babel-helper-explode-assignable-expression-6.24.1.tgz";
          sha1 = "f25b82cf7dc10433c55f70592d5746400ac22caa";
        };
      };
      "babel-helper-explode-class-6.24.1" = {
        name = "babel-helper-explode-class";
        packageName = "babel-helper-explode-class";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-explode-class/-/babel-helper-explode-class-6.24.1.tgz";
          sha1 = "7dc2a3910dee007056e1e31d640ced3d54eaa9eb";
        };
      };
      "babel-helper-function-name-6.24.1" = {
        name = "babel-helper-function-name";
        packageName = "babel-helper-function-name";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-function-name/-/babel-helper-function-name-6.24.1.tgz";
          sha1 = "d3475b8c03ed98242a25b48351ab18399d3580a9";
        };
      };
      "babel-helper-get-function-arity-6.24.1" = {
        name = "babel-helper-get-function-arity";
        packageName = "babel-helper-get-function-arity";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-get-function-arity/-/babel-helper-get-function-arity-6.24.1.tgz";
          sha1 = "8f7782aa93407c41d3aa50908f89b031b1b6853d";
        };
      };
      "babel-helper-hoist-variables-6.24.1" = {
        name = "babel-helper-hoist-variables";
        packageName = "babel-helper-hoist-variables";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-hoist-variables/-/babel-helper-hoist-variables-6.24.1.tgz";
          sha1 = "1ecb27689c9d25513eadbc9914a73f5408be7a76";
        };
      };
      "babel-helper-optimise-call-expression-6.24.1" = {
        name = "babel-helper-optimise-call-expression";
        packageName = "babel-helper-optimise-call-expression";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-optimise-call-expression/-/babel-helper-optimise-call-expression-6.24.1.tgz";
          sha1 = "f7a13427ba9f73f8f4fa993c54a97882d1244257";
        };
      };
      "babel-helper-regex-6.26.0" = {
        name = "babel-helper-regex";
        packageName = "babel-helper-regex";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-regex/-/babel-helper-regex-6.26.0.tgz";
          sha1 = "325c59f902f82f24b74faceed0363954f6495e72";
        };
      };
      "babel-helper-remap-async-to-generator-6.24.1" = {
        name = "babel-helper-remap-async-to-generator";
        packageName = "babel-helper-remap-async-to-generator";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-remap-async-to-generator/-/babel-helper-remap-async-to-generator-6.24.1.tgz";
          sha1 = "5ec581827ad723fecdd381f1c928390676e4551b";
        };
      };
      "babel-helper-replace-supers-6.24.1" = {
        name = "babel-helper-replace-supers";
        packageName = "babel-helper-replace-supers";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helper-replace-supers/-/babel-helper-replace-supers-6.24.1.tgz";
          sha1 = "bf6dbfe43938d17369a213ca8a8bf74b6a90ab1a";
        };
      };
      "babel-helpers-6.24.1" = {
        name = "babel-helpers";
        packageName = "babel-helpers";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-helpers/-/babel-helpers-6.24.1.tgz";
          sha1 = "3471de9caec388e5c850e597e58a26ddf37602b2";
        };
      };
      "babel-loader-7.1.4" = {
        name = "babel-loader";
        packageName = "babel-loader";
        version = "7.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-loader/-/babel-loader-7.1.4.tgz";
          sha512 = "21zk9kfpysnh8q94m4hymzd19i2r3pbmzw2gfj4b12ny9hw3sgrnlm8zkcm413aykwyl42ym27xw937fi2g6s2gls39n17kyc9g45py";
        };
      };
      "babel-messages-6.23.0" = {
        name = "babel-messages";
        packageName = "babel-messages";
        version = "6.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-messages/-/babel-messages-6.23.0.tgz";
          sha1 = "f3cdf4703858035b2a2951c6ec5edf6c62f2630e";
        };
      };
      "babel-plugin-check-es2015-constants-6.22.0" = {
        name = "babel-plugin-check-es2015-constants";
        packageName = "babel-plugin-check-es2015-constants";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-check-es2015-constants/-/babel-plugin-check-es2015-constants-6.22.0.tgz";
          sha1 = "35157b101426fd2ffd3da3f75c7d1e91835bbf8a";
        };
      };
      "babel-plugin-espower-2.4.0" = {
        name = "babel-plugin-espower";
        packageName = "babel-plugin-espower";
        version = "2.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-espower/-/babel-plugin-espower-2.4.0.tgz";
          sha512 = "1bb3hwgyqjviig07nxlsf15pnsy083vz7jx39cw8mdv6n9zl1bxsnkmfll3l3nvb2y1lxa007jqqjy2ypwis16avg408ap95skr3r7z";
        };
      };
      "babel-plugin-syntax-async-functions-6.13.0" = {
        name = "babel-plugin-syntax-async-functions";
        packageName = "babel-plugin-syntax-async-functions";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-async-functions/-/babel-plugin-syntax-async-functions-6.13.0.tgz";
          sha1 = "cad9cad1191b5ad634bf30ae0872391e0647be95";
        };
      };
      "babel-plugin-syntax-async-generators-6.13.0" = {
        name = "babel-plugin-syntax-async-generators";
        packageName = "babel-plugin-syntax-async-generators";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-async-generators/-/babel-plugin-syntax-async-generators-6.13.0.tgz";
          sha1 = "6bc963ebb16eccbae6b92b596eb7f35c342a8b9a";
        };
      };
      "babel-plugin-syntax-class-properties-6.13.0" = {
        name = "babel-plugin-syntax-class-properties";
        packageName = "babel-plugin-syntax-class-properties";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-class-properties/-/babel-plugin-syntax-class-properties-6.13.0.tgz";
          sha1 = "d7eb23b79a317f8543962c505b827c7d6cac27de";
        };
      };
      "babel-plugin-syntax-decorators-6.13.0" = {
        name = "babel-plugin-syntax-decorators";
        packageName = "babel-plugin-syntax-decorators";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-decorators/-/babel-plugin-syntax-decorators-6.13.0.tgz";
          sha1 = "312563b4dbde3cc806cee3e416cceeaddd11ac0b";
        };
      };
      "babel-plugin-syntax-dynamic-import-6.18.0" = {
        name = "babel-plugin-syntax-dynamic-import";
        packageName = "babel-plugin-syntax-dynamic-import";
        version = "6.18.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-dynamic-import/-/babel-plugin-syntax-dynamic-import-6.18.0.tgz";
          sha1 = "8d6a26229c83745a9982a441051572caa179b1da";
        };
      };
      "babel-plugin-syntax-exponentiation-operator-6.13.0" = {
        name = "babel-plugin-syntax-exponentiation-operator";
        packageName = "babel-plugin-syntax-exponentiation-operator";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-exponentiation-operator/-/babel-plugin-syntax-exponentiation-operator-6.13.0.tgz";
          sha1 = "9ee7e8337290da95288201a6a57f4170317830de";
        };
      };
      "babel-plugin-syntax-flow-6.18.0" = {
        name = "babel-plugin-syntax-flow";
        packageName = "babel-plugin-syntax-flow";
        version = "6.18.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-flow/-/babel-plugin-syntax-flow-6.18.0.tgz";
          sha1 = "4c3ab20a2af26aa20cd25995c398c4eb70310c8d";
        };
      };
      "babel-plugin-syntax-jsx-6.18.0" = {
        name = "babel-plugin-syntax-jsx";
        packageName = "babel-plugin-syntax-jsx";
        version = "6.18.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-jsx/-/babel-plugin-syntax-jsx-6.18.0.tgz";
          sha1 = "0af32a9a6e13ca7a3fd5069e62d7b0f58d0d8946";
        };
      };
      "babel-plugin-syntax-object-rest-spread-6.13.0" = {
        name = "babel-plugin-syntax-object-rest-spread";
        packageName = "babel-plugin-syntax-object-rest-spread";
        version = "6.13.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-object-rest-spread/-/babel-plugin-syntax-object-rest-spread-6.13.0.tgz";
          sha1 = "fd6536f2bce13836ffa3a5458c4903a597bb3bf5";
        };
      };
      "babel-plugin-syntax-trailing-function-commas-6.22.0" = {
        name = "babel-plugin-syntax-trailing-function-commas";
        packageName = "babel-plugin-syntax-trailing-function-commas";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-syntax-trailing-function-commas/-/babel-plugin-syntax-trailing-function-commas-6.22.0.tgz";
          sha1 = "ba0360937f8d06e40180a43fe0d5616fff532cf3";
        };
      };
      "babel-plugin-transform-async-generator-functions-6.24.1" = {
        name = "babel-plugin-transform-async-generator-functions";
        packageName = "babel-plugin-transform-async-generator-functions";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-async-generator-functions/-/babel-plugin-transform-async-generator-functions-6.24.1.tgz";
          sha1 = "f058900145fd3e9907a6ddf28da59f215258a5db";
        };
      };
      "babel-plugin-transform-async-to-generator-6.24.1" = {
        name = "babel-plugin-transform-async-to-generator";
        packageName = "babel-plugin-transform-async-to-generator";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-async-to-generator/-/babel-plugin-transform-async-to-generator-6.24.1.tgz";
          sha1 = "6536e378aff6cb1d5517ac0e40eb3e9fc8d08761";
        };
      };
      "babel-plugin-transform-class-properties-6.24.1" = {
        name = "babel-plugin-transform-class-properties";
        packageName = "babel-plugin-transform-class-properties";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-class-properties/-/babel-plugin-transform-class-properties-6.24.1.tgz";
          sha1 = "6a79763ea61d33d36f37b611aa9def81a81b46ac";
        };
      };
      "babel-plugin-transform-decorators-6.24.1" = {
        name = "babel-plugin-transform-decorators";
        packageName = "babel-plugin-transform-decorators";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-decorators/-/babel-plugin-transform-decorators-6.24.1.tgz";
          sha1 = "788013d8f8c6b5222bdf7b344390dfd77569e24d";
        };
      };
      "babel-plugin-transform-es2015-arrow-functions-6.22.0" = {
        name = "babel-plugin-transform-es2015-arrow-functions";
        packageName = "babel-plugin-transform-es2015-arrow-functions";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-arrow-functions/-/babel-plugin-transform-es2015-arrow-functions-6.22.0.tgz";
          sha1 = "452692cb711d5f79dc7f85e440ce41b9f244d221";
        };
      };
      "babel-plugin-transform-es2015-block-scoped-functions-6.22.0" = {
        name = "babel-plugin-transform-es2015-block-scoped-functions";
        packageName = "babel-plugin-transform-es2015-block-scoped-functions";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-block-scoped-functions/-/babel-plugin-transform-es2015-block-scoped-functions-6.22.0.tgz";
          sha1 = "bbc51b49f964d70cb8d8e0b94e820246ce3a6141";
        };
      };
      "babel-plugin-transform-es2015-block-scoping-6.26.0" = {
        name = "babel-plugin-transform-es2015-block-scoping";
        packageName = "babel-plugin-transform-es2015-block-scoping";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-block-scoping/-/babel-plugin-transform-es2015-block-scoping-6.26.0.tgz";
          sha1 = "d70f5299c1308d05c12f463813b0a09e73b1895f";
        };
      };
      "babel-plugin-transform-es2015-classes-6.24.1" = {
        name = "babel-plugin-transform-es2015-classes";
        packageName = "babel-plugin-transform-es2015-classes";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-classes/-/babel-plugin-transform-es2015-classes-6.24.1.tgz";
          sha1 = "5a4c58a50c9c9461e564b4b2a3bfabc97a2584db";
        };
      };
      "babel-plugin-transform-es2015-computed-properties-6.24.1" = {
        name = "babel-plugin-transform-es2015-computed-properties";
        packageName = "babel-plugin-transform-es2015-computed-properties";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-computed-properties/-/babel-plugin-transform-es2015-computed-properties-6.24.1.tgz";
          sha1 = "6fe2a8d16895d5634f4cd999b6d3480a308159b3";
        };
      };
      "babel-plugin-transform-es2015-destructuring-6.23.0" = {
        name = "babel-plugin-transform-es2015-destructuring";
        packageName = "babel-plugin-transform-es2015-destructuring";
        version = "6.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-destructuring/-/babel-plugin-transform-es2015-destructuring-6.23.0.tgz";
          sha1 = "997bb1f1ab967f682d2b0876fe358d60e765c56d";
        };
      };
      "babel-plugin-transform-es2015-duplicate-keys-6.24.1" = {
        name = "babel-plugin-transform-es2015-duplicate-keys";
        packageName = "babel-plugin-transform-es2015-duplicate-keys";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-duplicate-keys/-/babel-plugin-transform-es2015-duplicate-keys-6.24.1.tgz";
          sha1 = "73eb3d310ca969e3ef9ec91c53741a6f1576423e";
        };
      };
      "babel-plugin-transform-es2015-for-of-6.23.0" = {
        name = "babel-plugin-transform-es2015-for-of";
        packageName = "babel-plugin-transform-es2015-for-of";
        version = "6.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-for-of/-/babel-plugin-transform-es2015-for-of-6.23.0.tgz";
          sha1 = "f47c95b2b613df1d3ecc2fdb7573623c75248691";
        };
      };
      "babel-plugin-transform-es2015-function-name-6.24.1" = {
        name = "babel-plugin-transform-es2015-function-name";
        packageName = "babel-plugin-transform-es2015-function-name";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-function-name/-/babel-plugin-transform-es2015-function-name-6.24.1.tgz";
          sha1 = "834c89853bc36b1af0f3a4c5dbaa94fd8eacaa8b";
        };
      };
      "babel-plugin-transform-es2015-literals-6.22.0" = {
        name = "babel-plugin-transform-es2015-literals";
        packageName = "babel-plugin-transform-es2015-literals";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-literals/-/babel-plugin-transform-es2015-literals-6.22.0.tgz";
          sha1 = "4f54a02d6cd66cf915280019a31d31925377ca2e";
        };
      };
      "babel-plugin-transform-es2015-modules-amd-6.24.1" = {
        name = "babel-plugin-transform-es2015-modules-amd";
        packageName = "babel-plugin-transform-es2015-modules-amd";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-modules-amd/-/babel-plugin-transform-es2015-modules-amd-6.24.1.tgz";
          sha1 = "3b3e54017239842d6d19c3011c4bd2f00a00d154";
        };
      };
      "babel-plugin-transform-es2015-modules-commonjs-6.26.0" = {
        name = "babel-plugin-transform-es2015-modules-commonjs";
        packageName = "babel-plugin-transform-es2015-modules-commonjs";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-modules-commonjs/-/babel-plugin-transform-es2015-modules-commonjs-6.26.0.tgz";
          sha1 = "0d8394029b7dc6abe1a97ef181e00758dd2e5d8a";
        };
      };
      "babel-plugin-transform-es2015-modules-systemjs-6.24.1" = {
        name = "babel-plugin-transform-es2015-modules-systemjs";
        packageName = "babel-plugin-transform-es2015-modules-systemjs";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-modules-systemjs/-/babel-plugin-transform-es2015-modules-systemjs-6.24.1.tgz";
          sha1 = "ff89a142b9119a906195f5f106ecf305d9407d23";
        };
      };
      "babel-plugin-transform-es2015-modules-umd-6.24.1" = {
        name = "babel-plugin-transform-es2015-modules-umd";
        packageName = "babel-plugin-transform-es2015-modules-umd";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-modules-umd/-/babel-plugin-transform-es2015-modules-umd-6.24.1.tgz";
          sha1 = "ac997e6285cd18ed6176adb607d602344ad38468";
        };
      };
      "babel-plugin-transform-es2015-object-super-6.24.1" = {
        name = "babel-plugin-transform-es2015-object-super";
        packageName = "babel-plugin-transform-es2015-object-super";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-object-super/-/babel-plugin-transform-es2015-object-super-6.24.1.tgz";
          sha1 = "24cef69ae21cb83a7f8603dad021f572eb278f8d";
        };
      };
      "babel-plugin-transform-es2015-parameters-6.24.1" = {
        name = "babel-plugin-transform-es2015-parameters";
        packageName = "babel-plugin-transform-es2015-parameters";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-parameters/-/babel-plugin-transform-es2015-parameters-6.24.1.tgz";
          sha1 = "57ac351ab49caf14a97cd13b09f66fdf0a625f2b";
        };
      };
      "babel-plugin-transform-es2015-shorthand-properties-6.24.1" = {
        name = "babel-plugin-transform-es2015-shorthand-properties";
        packageName = "babel-plugin-transform-es2015-shorthand-properties";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-shorthand-properties/-/babel-plugin-transform-es2015-shorthand-properties-6.24.1.tgz";
          sha1 = "24f875d6721c87661bbd99a4622e51f14de38aa0";
        };
      };
      "babel-plugin-transform-es2015-spread-6.22.0" = {
        name = "babel-plugin-transform-es2015-spread";
        packageName = "babel-plugin-transform-es2015-spread";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-spread/-/babel-plugin-transform-es2015-spread-6.22.0.tgz";
          sha1 = "d6d68a99f89aedc4536c81a542e8dd9f1746f8d1";
        };
      };
      "babel-plugin-transform-es2015-sticky-regex-6.24.1" = {
        name = "babel-plugin-transform-es2015-sticky-regex";
        packageName = "babel-plugin-transform-es2015-sticky-regex";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-sticky-regex/-/babel-plugin-transform-es2015-sticky-regex-6.24.1.tgz";
          sha1 = "00c1cdb1aca71112cdf0cf6126c2ed6b457ccdbc";
        };
      };
      "babel-plugin-transform-es2015-template-literals-6.22.0" = {
        name = "babel-plugin-transform-es2015-template-literals";
        packageName = "babel-plugin-transform-es2015-template-literals";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-template-literals/-/babel-plugin-transform-es2015-template-literals-6.22.0.tgz";
          sha1 = "a84b3450f7e9f8f1f6839d6d687da84bb1236d8d";
        };
      };
      "babel-plugin-transform-es2015-typeof-symbol-6.23.0" = {
        name = "babel-plugin-transform-es2015-typeof-symbol";
        packageName = "babel-plugin-transform-es2015-typeof-symbol";
        version = "6.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-typeof-symbol/-/babel-plugin-transform-es2015-typeof-symbol-6.23.0.tgz";
          sha1 = "dec09f1cddff94b52ac73d505c84df59dcceb372";
        };
      };
      "babel-plugin-transform-es2015-unicode-regex-6.24.1" = {
        name = "babel-plugin-transform-es2015-unicode-regex";
        packageName = "babel-plugin-transform-es2015-unicode-regex";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-es2015-unicode-regex/-/babel-plugin-transform-es2015-unicode-regex-6.24.1.tgz";
          sha1 = "d38b12f42ea7323f729387f18a7c5ae1faeb35e9";
        };
      };
      "babel-plugin-transform-exponentiation-operator-6.24.1" = {
        name = "babel-plugin-transform-exponentiation-operator";
        packageName = "babel-plugin-transform-exponentiation-operator";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-exponentiation-operator/-/babel-plugin-transform-exponentiation-operator-6.24.1.tgz";
          sha1 = "2ab0c9c7f3098fa48907772bb813fe41e8de3a0e";
        };
      };
      "babel-plugin-transform-flow-strip-types-6.22.0" = {
        name = "babel-plugin-transform-flow-strip-types";
        packageName = "babel-plugin-transform-flow-strip-types";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-flow-strip-types/-/babel-plugin-transform-flow-strip-types-6.22.0.tgz";
          sha1 = "84cb672935d43714fdc32bce84568d87441cf7cf";
        };
      };
      "babel-plugin-transform-object-rest-spread-6.26.0" = {
        name = "babel-plugin-transform-object-rest-spread";
        packageName = "babel-plugin-transform-object-rest-spread";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-object-rest-spread/-/babel-plugin-transform-object-rest-spread-6.26.0.tgz";
          sha1 = "0f36692d50fef6b7e2d4b3ac1478137a963b7b06";
        };
      };
      "babel-plugin-transform-react-display-name-6.25.0" = {
        name = "babel-plugin-transform-react-display-name";
        packageName = "babel-plugin-transform-react-display-name";
        version = "6.25.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-react-display-name/-/babel-plugin-transform-react-display-name-6.25.0.tgz";
          sha1 = "67e2bf1f1e9c93ab08db96792e05392bf2cc28d1";
        };
      };
      "babel-plugin-transform-react-jsx-6.24.1" = {
        name = "babel-plugin-transform-react-jsx";
        packageName = "babel-plugin-transform-react-jsx";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-react-jsx/-/babel-plugin-transform-react-jsx-6.24.1.tgz";
          sha1 = "840a028e7df460dfc3a2d29f0c0d91f6376e66a3";
        };
      };
      "babel-plugin-transform-react-jsx-self-6.22.0" = {
        name = "babel-plugin-transform-react-jsx-self";
        packageName = "babel-plugin-transform-react-jsx-self";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-react-jsx-self/-/babel-plugin-transform-react-jsx-self-6.22.0.tgz";
          sha1 = "df6d80a9da2612a121e6ddd7558bcbecf06e636e";
        };
      };
      "babel-plugin-transform-react-jsx-source-6.22.0" = {
        name = "babel-plugin-transform-react-jsx-source";
        packageName = "babel-plugin-transform-react-jsx-source";
        version = "6.22.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-react-jsx-source/-/babel-plugin-transform-react-jsx-source-6.22.0.tgz";
          sha1 = "66ac12153f5cd2d17b3c19268f4bf0197f44ecd6";
        };
      };
      "babel-plugin-transform-regenerator-6.26.0" = {
        name = "babel-plugin-transform-regenerator";
        packageName = "babel-plugin-transform-regenerator";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-regenerator/-/babel-plugin-transform-regenerator-6.26.0.tgz";
          sha1 = "e0703696fbde27f0a3efcacf8b4dca2f7b3a8f2f";
        };
      };
      "babel-plugin-transform-strict-mode-6.24.1" = {
        name = "babel-plugin-transform-strict-mode";
        packageName = "babel-plugin-transform-strict-mode";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-plugin-transform-strict-mode/-/babel-plugin-transform-strict-mode-6.24.1.tgz";
          sha1 = "d5faf7aa578a65bbe591cf5edae04a0c67020758";
        };
      };
      "babel-polyfill-6.26.0" = {
        name = "babel-polyfill";
        packageName = "babel-polyfill";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-polyfill/-/babel-polyfill-6.26.0.tgz";
          sha1 = "379937abc67d7895970adc621f284cd966cf2153";
        };
      };
      "babel-preset-es2015-6.24.1" = {
        name = "babel-preset-es2015";
        packageName = "babel-preset-es2015";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-preset-es2015/-/babel-preset-es2015-6.24.1.tgz";
          sha1 = "d44050d6bc2c9feea702aaf38d727a0210538939";
        };
      };
      "babel-preset-flow-6.23.0" = {
        name = "babel-preset-flow";
        packageName = "babel-preset-flow";
        version = "6.23.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-preset-flow/-/babel-preset-flow-6.23.0.tgz";
          sha1 = "e71218887085ae9a24b5be4169affb599816c49d";
        };
      };
      "babel-preset-react-6.24.1" = {
        name = "babel-preset-react";
        packageName = "babel-preset-react";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-preset-react/-/babel-preset-react-6.24.1.tgz";
          sha1 = "ba69dfaea45fc3ec639b6a4ecea6e17702c91380";
        };
      };
      "babel-preset-stage-2-6.24.1" = {
        name = "babel-preset-stage-2";
        packageName = "babel-preset-stage-2";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-preset-stage-2/-/babel-preset-stage-2-6.24.1.tgz";
          sha1 = "d9e2960fb3d71187f0e64eec62bc07767219bdc1";
        };
      };
      "babel-preset-stage-3-6.24.1" = {
        name = "babel-preset-stage-3";
        packageName = "babel-preset-stage-3";
        version = "6.24.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-preset-stage-3/-/babel-preset-stage-3-6.24.1.tgz";
          sha1 = "836ada0a9e7a7fa37cb138fb9326f87934a48395";
        };
      };
      "babel-register-6.26.0" = {
        name = "babel-register";
        packageName = "babel-register";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-register/-/babel-register-6.26.0.tgz";
          sha1 = "6ed021173e2fcb486d7acb45c6009a856f647071";
        };
      };
      "babel-runtime-6.26.0" = {
        name = "babel-runtime";
        packageName = "babel-runtime";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-runtime/-/babel-runtime-6.26.0.tgz";
          sha1 = "965c7058668e82b55d7bfe04ff2337bc8b5647fe";
        };
      };
      "babel-template-6.26.0" = {
        name = "babel-template";
        packageName = "babel-template";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-template/-/babel-template-6.26.0.tgz";
          sha1 = "de03e2d16396b069f46dd9fff8521fb1a0e35e02";
        };
      };
      "babel-traverse-6.26.0" = {
        name = "babel-traverse";
        packageName = "babel-traverse";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-traverse/-/babel-traverse-6.26.0.tgz";
          sha1 = "46a9cbd7edcc62c8e5c064e2d2d8d0f4035766ee";
        };
      };
      "babel-types-6.26.0" = {
        name = "babel-types";
        packageName = "babel-types";
        version = "6.26.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babel-types/-/babel-types-6.26.0.tgz";
          sha1 = "a3b073f94ab49eb6fa55cd65227a334380632497";
        };
      };
      "babylon-6.18.0" = {
        name = "babylon";
        packageName = "babylon";
        version = "6.18.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/babylon/-/babylon-6.18.0.tgz";
          sha512 = "1qk460vyxfs08g8586jdc02wqzyy2y06596qcn1na9bz7yxra6vgh6177qf345xai0virpaz56bkpgmfcrd8yx5l2vjkn49y66h9xdb";
        };
      };
      "backo2-1.0.2" = {
        name = "backo2";
        packageName = "backo2";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/backo2/-/backo2-1.0.2.tgz";
          sha1 = "31ab1ac8b129363463e35b3ebb69f4dfcfba7947";
        };
      };
      "balanced-match-0.1.0" = {
        name = "balanced-match";
        packageName = "balanced-match";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/balanced-match/-/balanced-match-0.1.0.tgz";
          sha1 = "b504bd05869b39259dd0c5efc35d843176dccc4a";
        };
      };
      "balanced-match-0.4.2" = {
        name = "balanced-match";
        packageName = "balanced-match";
        version = "0.4.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/balanced-match/-/balanced-match-0.4.2.tgz";
          sha1 = "cb3f3e3c732dc0f01ee70b403f302e61d7709838";
        };
      };
      "balanced-match-1.0.0" = {
        name = "balanced-match";
        packageName = "balanced-match";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.0.tgz";
          sha1 = "89b4d199ab2bee49de164ea02b89ce462d71b767";
        };
      };
      "base-0.11.2" = {
        name = "base";
        packageName = "base";
        version = "0.11.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/base/-/base-0.11.2.tgz";
          sha512 = "11dwi4v72034dqafp0qxsg8h6cpn92vv4vf909a9fybd69yfg6gqn4hhav6x59r1wbi8h1qlgfh9np0340mpljv1hc9v9p02giqygp5";
        };
      };
      "base64-arraybuffer-0.1.5" = {
        name = "base64-arraybuffer";
        packageName = "base64-arraybuffer";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/base64-arraybuffer/-/base64-arraybuffer-0.1.5.tgz";
          sha1 = "73926771923b5a19747ad666aa5cd4bf9c6e9ce8";
        };
      };
      "base64-js-1.2.3" = {
        name = "base64-js";
        packageName = "base64-js";
        version = "1.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/base64-js/-/base64-js-1.2.3.tgz";
          sha512 = "3kqp8hzql2ccdqf7vqizav1lrwp5gynn081718g9slxcs428sv02n037xb9hfgrqybbk4qacnk5mcv63z8fm3l4h6fi06xm8nqj3h1j";
        };
      };
      "batch-0.6.1" = {
        name = "batch";
        packageName = "batch";
        version = "0.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/batch/-/batch-0.6.1.tgz";
          sha1 = "dc34314f4e679318093fc760272525f94bf25c16";
        };
      };
      "bcrypt-pbkdf-1.0.1" = {
        name = "bcrypt-pbkdf";
        packageName = "bcrypt-pbkdf";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.1.tgz";
          sha1 = "63bc5dcb61331b92bc05fd528953c33462a06f8d";
        };
      };
      "better-assert-1.0.2" = {
        name = "better-assert";
        packageName = "better-assert";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/better-assert/-/better-assert-1.0.2.tgz";
          sha1 = "40866b9e1b9e0b55b481894311e68faffaebc522";
        };
      };
      "big.js-3.2.0" = {
        name = "big.js";
        packageName = "big.js";
        version = "3.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/big.js/-/big.js-3.2.0.tgz";
          sha512 = "3qicqys1bg16slzbzjn3f0fir82r4d1h6lvy5y0cqqwzbs2iaxf93xgi6x47m7l87i102ifjn4qvjbf764gyncsxcqw7lw33mk7y4zs";
        };
      };
      "bignumber.js-4.1.0" = {
        name = "bignumber.js";
        packageName = "bignumber.js";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/bignumber.js/-/bignumber.js-4.1.0.tgz";
          sha512 = "0ldjj2kpr5hz3js5jnxa9bcl1451nb6x111pk2mh6jrhdawwa2m2qgd8qicwgill6xmv5d57zh7jylz0mmwl6vvrl4vxx1jas8di73q";
        };
      };
      "binary-extensions-1.11.0" = {
        name = "binary-extensions";
        packageName = "binary-extensions";
        version = "1.11.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/binary-extensions/-/binary-extensions-1.11.0.tgz";
          sha1 = "46aa1751fb6a2f93ee5e689bb1087d4b14c6c205";
        };
      };
      "blob-0.0.4" = {
        name = "blob";
        packageName = "blob";
        version = "0.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/blob/-/blob-0.0.4.tgz";
          sha1 = "bcf13052ca54463f30f9fc7e95b9a47630a94921";
        };
      };
      "bluebird-3.5.1" = {
        name = "bluebird";
        packageName = "bluebird";
        version = "3.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/bluebird/-/bluebird-3.5.1.tgz";
          sha512 = "2631bhp784qng0ifbypsmvijn6kjfvkhq2335kdz8ix5qi3wb3lbpg94xjn1av2s6i95ygr5a4y9j1721dw6zdbywwh1m48by4qpa1h";
        };
      };
      "bn.js-4.11.8" = {
        name = "bn.js";
        packageName = "bn.js";
        version = "4.11.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/bn.js/-/bn.js-4.11.8.tgz";
          sha512 = "20bg51v29zygy89w84qb64pkjikxfjdsgjs0ry6pvv8fkwn5kd1izrqn022d838q3rcaq8dmy033g7q8b6960j4f8ipan74y9ydimr2";
        };
      };
      "body-parser-1.18.2" = {
        name = "body-parser";
        packageName = "body-parser";
        version = "1.18.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/body-parser/-/body-parser-1.18.2.tgz";
          sha1 = "87678a19d84b47d859b83199bd59bce222b10454";
        };
      };
      "bonjour-3.5.0" = {
        name = "bonjour";
        packageName = "bonjour";
        version = "3.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/bonjour/-/bonjour-3.5.0.tgz";
          sha1 = "8e890a183d8ee9a2393b3844c691a42bcf7bc9f5";
        };
      };
      "boolbase-1.0.0" = {
        name = "boolbase";
        packageName = "boolbase";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/boolbase/-/boolbase-1.0.0.tgz";
          sha1 = "68dff5fbe60c51eb37725ea9e3ed310dcc1e776e";
        };
      };
      "boom-4.3.1" = {
        name = "boom";
        packageName = "boom";
        version = "4.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/boom/-/boom-4.3.1.tgz";
          sha1 = "4f8a3005cb4a7e3889f749030fd25b96e01d2e31";
        };
      };
      "boom-5.2.0" = {
        name = "boom";
        packageName = "boom";
        version = "5.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/boom/-/boom-5.2.0.tgz";
          sha512 = "19h20yqpvca08dns1rs4f057f10w63v0snxfml4h5khsk266x3x1im0w72bza4k2xn0kfz6jlv001dhcvxsjr09bmbqnysils9m7437";
        };
      };
      "bower-1.8.2" = {
        name = "bower";
        packageName = "bower";
        version = "1.8.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/bower/-/bower-1.8.2.tgz";
          sha1 = "adf53529c8d4af02ef24fb8d5341c1419d33e2f7";
        };
      };
      "boxen-1.3.0" = {
        name = "boxen";
        packageName = "boxen";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/boxen/-/boxen-1.3.0.tgz";
          sha512 = "0pmn5jcnph7yfgfhlncg1lys066cq44kavj4d9qhmyy9705w61pabpwlma09xg4xplzbxh78d3m4xwvjwk478r3xyqnmpzq79yy7lsc";
        };
      };
      "brace-expansion-1.1.11" = {
        name = "brace-expansion";
        packageName = "brace-expansion";
        version = "1.1.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
          sha512 = "248cnpbbf0p32h53rd3g8wzpgrkaj4p078ra1g6l16f82i6bzkvmhwqan5rk88apbll9ly1476kngd7f7z27i3b3zxpbb3064f8yaw8";
        };
      };
      "braces-1.8.5" = {
        name = "braces";
        packageName = "braces";
        version = "1.8.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/braces/-/braces-1.8.5.tgz";
          sha1 = "ba77962e12dff969d6b76711e914b737857bf6a7";
        };
      };
      "braces-2.3.1" = {
        name = "braces";
        packageName = "braces";
        version = "2.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/braces/-/braces-2.3.1.tgz";
          sha512 = "2fpcx8h100fhpaffr6mh5ysp6dv06nl5xkii807fw0c603i15sg3wsxmj63fdn4apxdnn8an2ks883mvrdsv7mfryhfxg1pf1h6bvj8";
        };
      };
      "brorand-1.1.0" = {
        name = "brorand";
        packageName = "brorand";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/brorand/-/brorand-1.1.0.tgz";
          sha1 = "12c25efe40a45e3c323eb8675a0a0ce57b22371f";
        };
      };
      "browser-process-hrtime-0.1.2" = {
        name = "browser-process-hrtime";
        packageName = "browser-process-hrtime";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/browser-process-hrtime/-/browser-process-hrtime-0.1.2.tgz";
          sha1 = "425d68a58d3447f02a04aa894187fce8af8b7b8e";
        };
      };
      "browserify-aes-1.1.1" = {
        name = "browserify-aes";
        packageName = "browserify-aes";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-aes/-/browserify-aes-1.1.1.tgz";
          sha512 = "0b874c5j68a6h1smd9avnc98zpjy2b4sykkhfpn97lzg7k5aq3ab0jdsmxjafifm0sa3srwscfpcl70gwnlg242p7cavnf115hd6sah";
        };
      };
      "browserify-cipher-1.0.0" = {
        name = "browserify-cipher";
        packageName = "browserify-cipher";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-cipher/-/browserify-cipher-1.0.0.tgz";
          sha1 = "9988244874bf5ed4e28da95666dcd66ac8fc363a";
        };
      };
      "browserify-des-1.0.0" = {
        name = "browserify-des";
        packageName = "browserify-des";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-des/-/browserify-des-1.0.0.tgz";
          sha1 = "daa277717470922ed2fe18594118a175439721dd";
        };
      };
      "browserify-rsa-4.0.1" = {
        name = "browserify-rsa";
        packageName = "browserify-rsa";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-rsa/-/browserify-rsa-4.0.1.tgz";
          sha1 = "21e0abfaf6f2029cf2fafb133567a701d4135524";
        };
      };
      "browserify-sign-4.0.4" = {
        name = "browserify-sign";
        packageName = "browserify-sign";
        version = "4.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-sign/-/browserify-sign-4.0.4.tgz";
          sha1 = "aa4eb68e5d7b658baa6bf6a57e630cbd7a93d298";
        };
      };
      "browserify-zlib-0.2.0" = {
        name = "browserify-zlib";
        packageName = "browserify-zlib";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserify-zlib/-/browserify-zlib-0.2.0.tgz";
          sha512 = "24488d4s6d901hj9d9jdddapmcvmibbdpjq6nv3bpyjx72546fcqa0vripy0ydsrw1jk6bakfzvynh5i9cz0g59hrmn4ph75d3kdpk7";
        };
      };
      "browserslist-1.7.7" = {
        name = "browserslist";
        packageName = "browserslist";
        version = "1.7.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserslist/-/browserslist-1.7.7.tgz";
          sha1 = "0bd76704258be829b2398bb50e4b62d1a166b0b9";
        };
      };
      "browserslist-2.11.3" = {
        name = "browserslist";
        packageName = "browserslist";
        version = "2.11.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/browserslist/-/browserslist-2.11.3.tgz";
          sha512 = "2c52470l3xpcqvzj8qpkwpb73gx4mq30wsk08n90waahj8g8qwd8cjmvf625f4836p39jiigrd0g4xh9k4p7mhcaykgw0pvfiqvjsy9";
        };
      };
      "buf-compare-1.0.1" = {
        name = "buf-compare";
        packageName = "buf-compare";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/buf-compare/-/buf-compare-1.0.1.tgz";
          sha1 = "fef28da8b8113a0a0db4430b0b6467b69730b34a";
        };
      };
      "buffer-4.9.1" = {
        name = "buffer";
        packageName = "buffer";
        version = "4.9.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/buffer/-/buffer-4.9.1.tgz";
          sha1 = "6d1bb601b07a4efced97094132093027c95bc298";
        };
      };
      "buffer-indexof-1.1.1" = {
        name = "buffer-indexof";
        packageName = "buffer-indexof";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/buffer-indexof/-/buffer-indexof-1.1.1.tgz";
          sha512 = "3bgz1zhq9ng3gypq825f00p9qi9y6z7wvkkf28nhjlyifnb3lk1dkmbya84k0ja79zv8kmmhvalwcnnz92533ip7pnjp3is1w9cxyp3";
        };
      };
      "buffer-xor-1.0.3" = {
        name = "buffer-xor";
        packageName = "buffer-xor";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/buffer-xor/-/buffer-xor-1.0.3.tgz";
          sha1 = "26e61ed1422fb70dd42e6e36729ed51d855fe8d9";
        };
      };
      "builtin-modules-1.1.1" = {
        name = "builtin-modules";
        packageName = "builtin-modules";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/builtin-modules/-/builtin-modules-1.1.1.tgz";
          sha1 = "270f076c5a72c02f5b65a47df94c5fe3a278892f";
        };
      };
      "builtin-status-codes-3.0.0" = {
        name = "builtin-status-codes";
        packageName = "builtin-status-codes";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/builtin-status-codes/-/builtin-status-codes-3.0.0.tgz";
          sha1 = "85982878e21b98e1c66425e03d0174788f569ee8";
        };
      };
      "bytes-3.0.0" = {
        name = "bytes";
        packageName = "bytes";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/bytes/-/bytes-3.0.0.tgz";
          sha1 = "d32815404d689699f85a4ea4fa8755dd13a96048";
        };
      };
      "cacache-10.0.4" = {
        name = "cacache";
        packageName = "cacache";
        version = "10.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/cacache/-/cacache-10.0.4.tgz";
          sha512 = "0f1zwi8bbiia995410m24xljd0ra0vk579q7rk48p7mrq9nbilx3qa2rnp7hjdwnvx28zfad0rfdcwyq3blzkb3rk8b9yl77crp960f";
        };
      };
      "cache-base-1.0.1" = {
        name = "cache-base";
        packageName = "cache-base";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/cache-base/-/cache-base-1.0.1.tgz";
          sha512 = "36i943khi87af4gif9r6imjgybqxq9cbd69z2h8p2s2j6scfbhrv7j3n591xl982fmyq29rkwh70a6qdcf3v0piwzfh8n2jf571v9q0";
        };
      };
      "caching-transform-1.0.1" = {
        name = "caching-transform";
        packageName = "caching-transform";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/caching-transform/-/caching-transform-1.0.1.tgz";
          sha1 = "6dbdb2f20f8d8fbce79f3e94e9d1742dcdf5c0a1";
        };
      };
      "call-matcher-1.0.1" = {
        name = "call-matcher";
        packageName = "call-matcher";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/call-matcher/-/call-matcher-1.0.1.tgz";
          sha1 = "5134d077984f712a54dad3cbf62de28dce416ca8";
        };
      };
      "call-signature-0.0.2" = {
        name = "call-signature";
        packageName = "call-signature";
        version = "0.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/call-signature/-/call-signature-0.0.2.tgz";
          sha1 = "a84abc825a55ef4cb2b028bd74e205a65b9a4996";
        };
      };
      "callsite-1.0.0" = {
        name = "callsite";
        packageName = "callsite";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/callsite/-/callsite-1.0.0.tgz";
          sha1 = "280398e5d664bd74038b6f0905153e6e8af1bc20";
        };
      };
      "camel-case-3.0.0" = {
        name = "camel-case";
        packageName = "camel-case";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/camel-case/-/camel-case-3.0.0.tgz";
          sha1 = "ca3c3688a4e9cf3a4cda777dc4dcbc713249cf73";
        };
      };
      "camelcase-1.2.1" = {
        name = "camelcase";
        packageName = "camelcase";
        version = "1.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/camelcase/-/camelcase-1.2.1.tgz";
          sha1 = "9bb5304d2e0b56698b2c758b08a3eaa9daa58a39";
        };
      };
      "camelcase-2.1.1" = {
        name = "camelcase";
        packageName = "camelcase";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/camelcase/-/camelcase-2.1.1.tgz";
          sha1 = "7c1d16d679a1bbe59ca02cacecfb011e201f5a1f";
        };
      };
      "camelcase-3.0.0" = {
        name = "camelcase";
        packageName = "camelcase";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/camelcase/-/camelcase-3.0.0.tgz";
          sha1 = "32fc4b9fcdaf845fcdf7e73bb97cac2261f0ab0a";
        };
      };
      "camelcase-4.1.0" = {
        name = "camelcase";
        packageName = "camelcase";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/camelcase/-/camelcase-4.1.0.tgz";
          sha1 = "d545635be1e33c542649c69173e5de6acfae34dd";
        };
      };
      "camelcase-keys-2.1.0" = {
        name = "camelcase-keys";
        packageName = "camelcase-keys";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/camelcase-keys/-/camelcase-keys-2.1.0.tgz";
          sha1 = "308beeaffdf28119051efa1d932213c91b8f92e7";
        };
      };
      "caniuse-api-1.6.1" = {
        name = "caniuse-api";
        packageName = "caniuse-api";
        version = "1.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/caniuse-api/-/caniuse-api-1.6.1.tgz";
          sha1 = "b534e7c734c4f81ec5fbe8aca2ad24354b962c6c";
        };
      };
      "caniuse-api-2.0.0" = {
        name = "caniuse-api";
        packageName = "caniuse-api";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/caniuse-api/-/caniuse-api-2.0.0.tgz";
          sha1 = "b1ddb5a5966b16f48dc4998444d4bbc6c7d9d834";
        };
      };
      "caniuse-db-1.0.30000813" = {
        name = "caniuse-db";
        packageName = "caniuse-db";
        version = "1.0.30000813";
        src = fetchurl {
          url = "https://registry.npmjs.org/caniuse-db/-/caniuse-db-1.0.30000813.tgz";
          sha1 = "e0a1c603f8880ad787b2a35652b2733f32a5e29a";
        };
      };
      "caniuse-lite-1.0.30000813" = {
        name = "caniuse-lite";
        packageName = "caniuse-lite";
        version = "1.0.30000813";
        src = fetchurl {
          url = "https://registry.npmjs.org/caniuse-lite/-/caniuse-lite-1.0.30000813.tgz";
          sha512 = "3w71j0i9qmmp17j2cagrsdpfzhds29l29nmxgp1sgiqll9mfkgw5xky4zy6lbbgjjahkkag7vdv23wf424d7fyh2m65fj7rc5517hh3";
        };
      };
      "capture-stack-trace-1.0.0" = {
        name = "capture-stack-trace";
        packageName = "capture-stack-trace";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/capture-stack-trace/-/capture-stack-trace-1.0.0.tgz";
          sha1 = "4a6fa07399c26bba47f0b2496b4d0fb408c5550d";
        };
      };
      "caseless-0.12.0" = {
        name = "caseless";
        packageName = "caseless";
        version = "0.12.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/caseless/-/caseless-0.12.0.tgz";
          sha1 = "1b681c21ff84033c826543090689420d187151dc";
        };
      };
      "center-align-0.1.3" = {
        name = "center-align";
        packageName = "center-align";
        version = "0.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/center-align/-/center-align-0.1.3.tgz";
          sha1 = "aa0d32629b6ee972200411cbd4461c907bc2b7ad";
        };
      };
      "chalk-0.4.0" = {
        name = "chalk";
        packageName = "chalk";
        version = "0.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/chalk/-/chalk-0.4.0.tgz";
          sha1 = "5199a3ddcd0c1efe23bc08c1b027b06176e0c64f";
        };
      };
      "chalk-1.1.3" = {
        name = "chalk";
        packageName = "chalk";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/chalk/-/chalk-1.1.3.tgz";
          sha1 = "a8115c55e4a702fe4d150abd3872822a7e09fc98";
        };
      };
      "chalk-2.3.2" = {
        name = "chalk";
        packageName = "chalk";
        version = "2.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/chalk/-/chalk-2.3.2.tgz";
          sha512 = "06jlrzx0nb92910rcfhx55n28jgvhc0qda49scnfyifnmc31dyfqsl5qqiwhsxkrhrc6c07x69q037f1pwg06kkfd1qdzaxz7dj7kk4";
        };
      };
      "chokidar-1.7.0" = {
        name = "chokidar";
        packageName = "chokidar";
        version = "1.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/chokidar/-/chokidar-1.7.0.tgz";
          sha1 = "798e689778151c8076b4b360e5edd28cda2bb468";
        };
      };
      "chokidar-2.0.2" = {
        name = "chokidar";
        packageName = "chokidar";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/chokidar/-/chokidar-2.0.2.tgz";
          sha512 = "1kncf65ax274b43r63m0bfbap3j5ah7dn86niqwpzzw6ilvpc7yy0fan14k584lw8ycf4nl78n5gv3bpyr4jrk5j3v441ragk1qfzcp";
        };
      };
      "chownr-1.0.1" = {
        name = "chownr";
        packageName = "chownr";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/chownr/-/chownr-1.0.1.tgz";
          sha1 = "e2a75042a9551908bebd25b8523d5f9769d79181";
        };
      };
      "ci-info-1.1.2" = {
        name = "ci-info";
        packageName = "ci-info";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/ci-info/-/ci-info-1.1.2.tgz";
          sha512 = "1jbmihk48iby72h0b6k4rvhrnaydml49qyjcb83ix310ivjzd4zmdk3yxx1ssn6ryjblm7xzaswnwj53rxwcyn1fr0jm7bzvhy8hcdr";
        };
      };
      "cipher-base-1.0.4" = {
        name = "cipher-base";
        packageName = "cipher-base";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/cipher-base/-/cipher-base-1.0.4.tgz";
          sha512 = "3cm9kdc1sv7pakzlhrc1pazdvg9lk4hv31lximwbcrgmwfzg6imxrndszgx9yzlizknfh2b73cr7b5mfcv50bldpyq6jr5s4zknsj1a";
        };
      };
      "clap-1.2.3" = {
        name = "clap";
        packageName = "clap";
        version = "1.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/clap/-/clap-1.2.3.tgz";
          sha512 = "1ha66pbxhll7c6vv641rahzq0ylwaifskwpwggy9k4sfh8r9n0r8mpqbib22dppb7zfrk6a84a4dyaal7mqs12jvlaxszz11py0nap0";
        };
      };
      "class-utils-0.3.6" = {
        name = "class-utils";
        packageName = "class-utils";
        version = "0.3.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/class-utils/-/class-utils-0.3.6.tgz";
          sha512 = "1xcqwmfmsbrm2ck76brwiqjmcza655khgh5szh6wngk357i37sgwsga1pbarwzaz9hvzkriqhq6j0z5mv0pmz61cf9wxvk3y5mlzs58";
        };
      };
      "clean-css-4.1.11" = {
        name = "clean-css";
        packageName = "clean-css";
        version = "4.1.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/clean-css/-/clean-css-4.1.11.tgz";
          sha1 = "2ecdf145aba38f54740f26cefd0ff3e03e125d6a";
        };
      };
      "clean-stack-1.3.0" = {
        name = "clean-stack";
        packageName = "clean-stack";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/clean-stack/-/clean-stack-1.3.0.tgz";
          sha1 = "9e821501ae979986c46b1d66d2d432db2fd4ae31";
        };
      };
      "clean-yaml-object-0.1.0" = {
        name = "clean-yaml-object";
        packageName = "clean-yaml-object";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/clean-yaml-object/-/clean-yaml-object-0.1.0.tgz";
          sha1 = "63fb110dc2ce1a84dc21f6d9334876d010ae8b68";
        };
      };
      "cli-boxes-1.0.0" = {
        name = "cli-boxes";
        packageName = "cli-boxes";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cli-boxes/-/cli-boxes-1.0.0.tgz";
          sha1 = "4fa917c3e59c94a004cd61f8ee509da651687143";
        };
      };
      "cli-cursor-2.1.0" = {
        name = "cli-cursor";
        packageName = "cli-cursor";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cli-cursor/-/cli-cursor-2.1.0.tgz";
          sha1 = "b35dac376479facc3e94747d41d0d0f5238ffcb5";
        };
      };
      "cli-spinners-1.1.0" = {
        name = "cli-spinners";
        packageName = "cli-spinners";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cli-spinners/-/cli-spinners-1.1.0.tgz";
          sha1 = "f1847b168844d917a671eb9d147e3df497c90d06";
        };
      };
      "cli-truncate-1.1.0" = {
        name = "cli-truncate";
        packageName = "cli-truncate";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cli-truncate/-/cli-truncate-1.1.0.tgz";
          sha512 = "1h48346i2bsfvj3h0qfxmyh1770cxb3d9ibk75yjag1xgzk021yqbmkiv30k5c0qgyb0sxkvjc3sckmakf4i7q1d2gh1nmw9fimj2vc";
        };
      };
      "cliui-2.1.0" = {
        name = "cliui";
        packageName = "cliui";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cliui/-/cliui-2.1.0.tgz";
          sha1 = "4b475760ff80264c762c3a1719032e91c7fea0d1";
        };
      };
      "cliui-3.2.0" = {
        name = "cliui";
        packageName = "cliui";
        version = "3.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cliui/-/cliui-3.2.0.tgz";
          sha1 = "120601537a916d29940f934da3b48d585a39213d";
        };
      };
      "cliui-4.0.0" = {
        name = "cliui";
        packageName = "cliui";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cliui/-/cliui-4.0.0.tgz";
          sha512 = "0mh539939k4z2nhj5h1m8kdr3bfy2f1kmdkss02cdbyabmpdkc6m22llyykymriahf54gpx6qg9v3vrs51gqgrrfhpsgbdndgjdd3cx";
        };
      };
      "clone-1.0.3" = {
        name = "clone";
        packageName = "clone";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/clone/-/clone-1.0.3.tgz";
          sha1 = "298d7e2231660f40c003c2ed3140decf3f53085f";
        };
      };
      "co-4.6.0" = {
        name = "co";
        packageName = "co";
        version = "4.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/co/-/co-4.6.0.tgz";
          sha1 = "6ea6bdf3d853ae54ccb8e47bfa0bf3f9031fb184";
        };
      };
      "co-with-promise-4.6.0" = {
        name = "co-with-promise";
        packageName = "co-with-promise";
        version = "4.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/co-with-promise/-/co-with-promise-4.6.0.tgz";
          sha1 = "413e7db6f5893a60b942cf492c4bec93db415ab7";
        };
      };
      "coa-1.0.4" = {
        name = "coa";
        packageName = "coa";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/coa/-/coa-1.0.4.tgz";
          sha1 = "a9ef153660d6a86a8bdec0289a5c684d217432fd";
        };
      };
      "code-excerpt-2.1.1" = {
        name = "code-excerpt";
        packageName = "code-excerpt";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/code-excerpt/-/code-excerpt-2.1.1.tgz";
          sha512 = "0kph3d5aw197lld3cqxhv6dw2rnmk7dfxwsf918r1mkbs2lvlr3k7cfgjg5y221axv6j49r8m6wjrbs45nj2pmqqzsny5i9f4gy34ml";
        };
      };
      "code-point-at-1.1.0" = {
        name = "code-point-at";
        packageName = "code-point-at";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/code-point-at/-/code-point-at-1.1.0.tgz";
          sha1 = "0d070b4d043a5bea33a2f1a40e2edb3d9a4ccf77";
        };
      };
      "collection-visit-1.0.0" = {
        name = "collection-visit";
        packageName = "collection-visit";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/collection-visit/-/collection-visit-1.0.0.tgz";
          sha1 = "4bc0373c164bc3291b4d368c829cf1a80a59dca0";
        };
      };
      "color-0.11.4" = {
        name = "color";
        packageName = "color";
        version = "0.11.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/color/-/color-0.11.4.tgz";
          sha1 = "6d7b5c74fb65e841cd48792ad1ed5e07b904d764";
        };
      };
      "color-1.0.3" = {
        name = "color";
        packageName = "color";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/color/-/color-1.0.3.tgz";
          sha1 = "e48e832d85f14ef694fb468811c2d5cfe729b55d";
        };
      };
      "color-2.0.1" = {
        name = "color";
        packageName = "color";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/color/-/color-2.0.1.tgz";
          sha512 = "1gir7mfj6033amg78p7jvpj0nk2hw61hqd81r6x3a2qmgizbw3d89k0qk62680zhm9ypcx6c9p2cgwjvb8smxv0qgvblkwza9ah5ddr";
        };
      };
      "color-convert-1.9.1" = {
        name = "color-convert";
        packageName = "color-convert";
        version = "1.9.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/color-convert/-/color-convert-1.9.1.tgz";
          sha512 = "32rj1090g95xcvm0d2ya6jbqdhiy9w2wv3picdy33fzrm455v0gi7g4n8lw0n31g37wwbdnz7lxjsisgbsaqz1d10j9nh5hi2f9lccs";
        };
      };
      "color-name-1.1.3" = {
        name = "color-name";
        packageName = "color-name";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz";
          sha1 = "a7d0558bd89c42f795dd42328f740831ca53bc25";
        };
      };
      "color-string-0.3.0" = {
        name = "color-string";
        packageName = "color-string";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/color-string/-/color-string-0.3.0.tgz";
          sha1 = "27d46fb67025c5c2fa25993bfbf579e47841b991";
        };
      };
      "color-string-1.5.2" = {
        name = "color-string";
        packageName = "color-string";
        version = "1.5.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/color-string/-/color-string-1.5.2.tgz";
          sha1 = "26e45814bc3c9a7cbd6751648a41434514a773a9";
        };
      };
      "colormin-1.1.2" = {
        name = "colormin";
        packageName = "colormin";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/colormin/-/colormin-1.1.2.tgz";
          sha1 = "ea2f7420a72b96881a38aae59ec124a6f7298133";
        };
      };
      "colors-1.1.2" = {
        name = "colors";
        packageName = "colors";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/colors/-/colors-1.1.2.tgz";
          sha1 = "168a4701756b6a7f51a12ce0c97bfa28c084ed63";
        };
      };
      "combined-stream-1.0.6" = {
        name = "combined-stream";
        packageName = "combined-stream";
        version = "1.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.6.tgz";
          sha1 = "723e7df6e801ac5613113a7e445a9b69cb632818";
        };
      };
      "commander-2.14.1" = {
        name = "commander";
        packageName = "commander";
        version = "2.14.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/commander/-/commander-2.14.1.tgz";
          sha512 = "11j7mwxfdnhi3yfkhxjvfkg1mzdsi3wq7iygma6ksmj1f7hfs0z8rpfcysqmy85f67mqvzhda3w1lyy1jqxxm31k799sazbipm7b17r";
        };
      };
      "common-path-prefix-1.0.0" = {
        name = "common-path-prefix";
        packageName = "common-path-prefix";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/common-path-prefix/-/common-path-prefix-1.0.0.tgz";
          sha1 = "cd52f6f0712e0baab97d6f9732874f22f47752c0";
        };
      };
      "commondir-1.0.1" = {
        name = "commondir";
        packageName = "commondir";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/commondir/-/commondir-1.0.1.tgz";
          sha1 = "ddd800da0c66127393cca5950ea968a3aaf1253b";
        };
      };
      "component-bind-1.0.0" = {
        name = "component-bind";
        packageName = "component-bind";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/component-bind/-/component-bind-1.0.0.tgz";
          sha1 = "00c608ab7dcd93897c0009651b1d3a8e1e73bbd1";
        };
      };
      "component-emitter-1.2.1" = {
        name = "component-emitter";
        packageName = "component-emitter";
        version = "1.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.2.1.tgz";
          sha1 = "137918d6d78283f7df7a6b7c5a63e140e69425e6";
        };
      };
      "component-inherit-0.0.3" = {
        name = "component-inherit";
        packageName = "component-inherit";
        version = "0.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/component-inherit/-/component-inherit-0.0.3.tgz";
          sha1 = "645fc4adf58b72b649d5cae65135619db26ff143";
        };
      };
      "compressible-2.0.13" = {
        name = "compressible";
        packageName = "compressible";
        version = "2.0.13";
        src = fetchurl {
          url = "https://registry.npmjs.org/compressible/-/compressible-2.0.13.tgz";
          sha1 = "0d1020ab924b2fdb4d6279875c7d6daba6baa7a9";
        };
      };
      "compression-1.7.2" = {
        name = "compression";
        packageName = "compression";
        version = "1.7.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/compression/-/compression-1.7.2.tgz";
          sha1 = "aaffbcd6aaf854b44ebb280353d5ad1651f59a69";
        };
      };
      "concat-map-0.0.1" = {
        name = "concat-map";
        packageName = "concat-map";
        version = "0.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
          sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
        };
      };
      "concat-stream-1.6.1" = {
        name = "concat-stream";
        packageName = "concat-stream";
        version = "1.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/concat-stream/-/concat-stream-1.6.1.tgz";
          sha512 = "1gyp2id8ifqdpzk81qz5q7200wf8m66ammg6cz8cxv4blmqh7rf761fxd1536ib4kcdp8jccvyx1l9vi6kjl26pwyd05pblki455jc2";
        };
      };
      "concordance-2.0.0" = {
        name = "concordance";
        packageName = "concordance";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/concordance/-/concordance-2.0.0.tgz";
          sha512 = "0yc5630sx79iv6i17yfirdzpx4dy6x3andqmxn42vb2bnia70sawahmrx2z9nc8p7y78rkxz5fgwid6s6z01hk1wm28c514n1jl2p4d";
        };
      };
      "concordance-3.0.0" = {
        name = "concordance";
        packageName = "concordance";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/concordance/-/concordance-3.0.0.tgz";
          sha512 = "34mwpz1g76z2x2dmapf5hi4c122jdps8fxh8ny61yi11c5rr27fiypk629p5qdl25afvhqkjgk8z4gv7dkd2dlkjmih5rg5gwkp7409";
        };
      };
      "configstore-3.1.1" = {
        name = "configstore";
        packageName = "configstore";
        version = "3.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/configstore/-/configstore-3.1.1.tgz";
          sha512 = "2zmidvkp20q25yv6a5d7k1daawdg0w6ppgayxzpwfhyvmgwybkkv7ni0j4b2j9c8wjn8z33zf5d4bjr8jywb5qixc75vypyy87n90z6";
        };
      };
      "connect-history-api-fallback-1.5.0" = {
        name = "connect-history-api-fallback";
        packageName = "connect-history-api-fallback";
        version = "1.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/connect-history-api-fallback/-/connect-history-api-fallback-1.5.0.tgz";
          sha1 = "b06873934bc5e344fef611a196a6faae0aee015a";
        };
      };
      "console-browserify-1.1.0" = {
        name = "console-browserify";
        packageName = "console-browserify";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/console-browserify/-/console-browserify-1.1.0.tgz";
          sha1 = "f0241c45730a9fc6323b206dbf38edc741d0bb10";
        };
      };
      "constants-browserify-1.0.0" = {
        name = "constants-browserify";
        packageName = "constants-browserify";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/constants-browserify/-/constants-browserify-1.0.0.tgz";
          sha1 = "c20b96d8c617748aaf1c16021760cd27fcb8cb75";
        };
      };
      "content-disposition-0.5.2" = {
        name = "content-disposition";
        packageName = "content-disposition";
        version = "0.5.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/content-disposition/-/content-disposition-0.5.2.tgz";
          sha1 = "0cf68bb9ddf5f2be7961c3a85178cb85dba78cb4";
        };
      };
      "content-type-1.0.4" = {
        name = "content-type";
        packageName = "content-type";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/content-type/-/content-type-1.0.4.tgz";
          sha512 = "1f4y61wc913jrnga7nny83gzf9l2488q6sl1ry9lbwgh5x5d3va0xcc0xrmjk6gdxl6d4r6rsk800xp5bazhjrx05yx1wpc8c8gg0w4";
        };
      };
      "content-type-parser-1.0.2" = {
        name = "content-type-parser";
        packageName = "content-type-parser";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/content-type-parser/-/content-type-parser-1.0.2.tgz";
          sha512 = "2jpr048rsw4crhj25318yi10rgmqqf3xkj3qvdh8mxd11cpia060mlrsw3a5adyh0501d25y8a1k70iikzgrsq13j5h64yc57h2bkll";
        };
      };
      "convert-source-map-1.5.1" = {
        name = "convert-source-map";
        packageName = "convert-source-map";
        version = "1.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.5.1.tgz";
          sha1 = "b8278097b9bc229365de5c62cf5fcaed8b5599e5";
        };
      };
      "convert-to-spaces-1.0.2" = {
        name = "convert-to-spaces";
        packageName = "convert-to-spaces";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/convert-to-spaces/-/convert-to-spaces-1.0.2.tgz";
          sha1 = "7e3e48bbe6d997b1417ddca2868204b4d3d85715";
        };
      };
      "cookie-0.3.1" = {
        name = "cookie";
        packageName = "cookie";
        version = "0.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/cookie/-/cookie-0.3.1.tgz";
          sha1 = "e7e0a1f9ef43b4c8ba925c5c5a96e806d16873bb";
        };
      };
      "cookie-signature-1.0.6" = {
        name = "cookie-signature";
        packageName = "cookie-signature";
        version = "1.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.0.6.tgz";
          sha1 = "e303a882b342cc3ee8ca513a79999734dab3ae2c";
        };
      };
      "copy-concurrently-1.0.5" = {
        name = "copy-concurrently";
        packageName = "copy-concurrently";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/copy-concurrently/-/copy-concurrently-1.0.5.tgz";
          sha512 = "3c1ggiqqnjgqlwdnimx94gm176c8rjsrih5qw2lbm642l8x7grx07v065k4j89c1p0adkm7v6sz11drb6j6sp51np2m1cazvycnhrvz";
        };
      };
      "copy-descriptor-0.1.1" = {
        name = "copy-descriptor";
        packageName = "copy-descriptor";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/copy-descriptor/-/copy-descriptor-0.1.1.tgz";
          sha1 = "676f6eb3c39997c2ee1ac3a924fd6124748f578d";
        };
      };
      "copy-webpack-plugin-4.5.1" = {
        name = "copy-webpack-plugin";
        packageName = "copy-webpack-plugin";
        version = "4.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/copy-webpack-plugin/-/copy-webpack-plugin-4.5.1.tgz";
          sha512 = "0sy90l4ni957mhmn95v0qn6ws8bbkyy0a30rmhign9r33acslbkkl7w2vrl8hf4v5a4rmrdhndhf1zpzfvphz71539pgl906vlfhm1s";
        };
      };
      "core-assert-0.2.1" = {
        name = "core-assert";
        packageName = "core-assert";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/core-assert/-/core-assert-0.2.1.tgz";
          sha1 = "f85e2cf9bfed28f773cc8b3fa5c5b69bdc02fe3f";
        };
      };
      "core-js-1.2.7" = {
        name = "core-js";
        packageName = "core-js";
        version = "1.2.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/core-js/-/core-js-1.2.7.tgz";
          sha1 = "652294c14651db28fa93bd2d5ff2983a4f08c636";
        };
      };
      "core-js-2.5.3" = {
        name = "core-js";
        packageName = "core-js";
        version = "2.5.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/core-js/-/core-js-2.5.3.tgz";
          sha1 = "8acc38345824f16d8365b7c9b4259168e8ed603e";
        };
      };
      "core-util-is-1.0.2" = {
        name = "core-util-is";
        packageName = "core-util-is";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz";
          sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
        };
      };
      "cosmiconfig-2.2.2" = {
        name = "cosmiconfig";
        packageName = "cosmiconfig";
        version = "2.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-2.2.2.tgz";
          sha512 = "3j2bwqqxpllfpdbnnjl2n380q8mcvz08p2xi5c7kzhqrscl8f3lmpa11jzsn76b5q2rz4604bffg2vxbbx4rrlmvgr82g8500pmf8qs";
        };
      };
      "create-ecdh-4.0.0" = {
        name = "create-ecdh";
        packageName = "create-ecdh";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/create-ecdh/-/create-ecdh-4.0.0.tgz";
          sha1 = "888c723596cdf7612f6498233eebd7a35301737d";
        };
      };
      "create-error-class-3.0.2" = {
        name = "create-error-class";
        packageName = "create-error-class";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/create-error-class/-/create-error-class-3.0.2.tgz";
          sha1 = "06be7abef947a3f14a30fd610671d401bca8b7b6";
        };
      };
      "create-hash-1.1.3" = {
        name = "create-hash";
        packageName = "create-hash";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/create-hash/-/create-hash-1.1.3.tgz";
          sha1 = "606042ac8b9262750f483caddab0f5819172d8fd";
        };
      };
      "create-hmac-1.1.6" = {
        name = "create-hmac";
        packageName = "create-hmac";
        version = "1.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/create-hmac/-/create-hmac-1.1.6.tgz";
          sha1 = "acb9e221a4e17bdb076e90657c42b93e3726cf06";
        };
      };
      "create-react-class-15.6.3" = {
        name = "create-react-class";
        packageName = "create-react-class";
        version = "15.6.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/create-react-class/-/create-react-class-15.6.3.tgz";
          sha512 = "0k42llsqz08six4w9hk8177czl4vjb1sgcm5kbzjld1nrn3a0xqgs858gvbfqyiz7b0wq6pynynfw60lfsw5khxcfxb631sl51zgvrk";
        };
      };
      "cross-spawn-3.0.1" = {
        name = "cross-spawn";
        packageName = "cross-spawn";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-3.0.1.tgz";
          sha1 = "1256037ecb9f0c5f79e3d6ef135e30770184b982";
        };
      };
      "cross-spawn-5.1.0" = {
        name = "cross-spawn";
        packageName = "cross-spawn";
        version = "5.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-5.1.0.tgz";
          sha1 = "e8bd0efee58fcff6f8f94510a0a554bbfa235449";
        };
      };
      "cryptiles-3.1.2" = {
        name = "cryptiles";
        packageName = "cryptiles";
        version = "3.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/cryptiles/-/cryptiles-3.1.2.tgz";
          sha1 = "a89fbb220f5ce25ec56e8c4aa8a4fd7b5b0d29fe";
        };
      };
      "crypto-browserify-3.12.0" = {
        name = "crypto-browserify";
        packageName = "crypto-browserify";
        version = "3.12.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/crypto-browserify/-/crypto-browserify-3.12.0.tgz";
          sha512 = "1d3mrhqlay037azmjp2ml5a8yyls9ijdhilv6f0znz0ajgfm972yr9bhm78wqi09p4crc3shgflk50jc63zijsqv777ikkyi2j2qgkz";
        };
      };
      "crypto-random-string-1.0.0" = {
        name = "crypto-random-string";
        packageName = "crypto-random-string";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/crypto-random-string/-/crypto-random-string-1.0.0.tgz";
          sha1 = "a230f64f568310e1498009940790ec99545bca7e";
        };
      };
      "css-color-function-1.3.3" = {
        name = "css-color-function";
        packageName = "css-color-function";
        version = "1.3.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-color-function/-/css-color-function-1.3.3.tgz";
          sha1 = "8ed24c2c0205073339fafa004bc8c141fccb282e";
        };
      };
      "css-color-names-0.0.4" = {
        name = "css-color-names";
        packageName = "css-color-names";
        version = "0.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-color-names/-/css-color-names-0.0.4.tgz";
          sha1 = "808adc2e79cf84738069b646cb20ec27beb629e0";
        };
      };
      "css-loader-0.28.10" = {
        name = "css-loader";
        packageName = "css-loader";
        version = "0.28.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-loader/-/css-loader-0.28.10.tgz";
          sha512 = "3jnk67qddnmnb2zvsfnxw9lpasqibihll5nqvpbd210dlaaf21s5f7sys2754dq9dsyszrbs23lf1pd3yfr8zmpkbjx4nx7washjljz";
        };
      };
      "css-select-1.2.0" = {
        name = "css-select";
        packageName = "css-select";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-select/-/css-select-1.2.0.tgz";
          sha1 = "2b3a110539c5355f1cd8d314623e870b121ec858";
        };
      };
      "css-selector-tokenizer-0.7.0" = {
        name = "css-selector-tokenizer";
        packageName = "css-selector-tokenizer";
        version = "0.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-selector-tokenizer/-/css-selector-tokenizer-0.7.0.tgz";
          sha1 = "e6988474ae8c953477bf5e7efecfceccd9cf4c86";
        };
      };
      "css-unit-converter-1.1.1" = {
        name = "css-unit-converter";
        packageName = "css-unit-converter";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-unit-converter/-/css-unit-converter-1.1.1.tgz";
          sha1 = "d9b9281adcfd8ced935bdbaba83786897f64e996";
        };
      };
      "css-what-2.1.0" = {
        name = "css-what";
        packageName = "css-what";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/css-what/-/css-what-2.1.0.tgz";
          sha1 = "9467d032c38cfaefb9f2d79501253062f87fa1bd";
        };
      };
      "cssesc-0.1.0" = {
        name = "cssesc";
        packageName = "cssesc";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cssesc/-/cssesc-0.1.0.tgz";
          sha1 = "c814903e45623371a0477b40109aaafbeeaddbb4";
        };
      };
      "cssnano-3.10.0" = {
        name = "cssnano";
        packageName = "cssnano";
        version = "3.10.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/cssnano/-/cssnano-3.10.0.tgz";
          sha1 = "4f38f6cea2b9b17fa01490f23f1dc68ea65c1c38";
        };
      };
      "csso-2.3.2" = {
        name = "csso";
        packageName = "csso";
        version = "2.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/csso/-/csso-2.3.2.tgz";
          sha1 = "ddd52c587033f49e94b71fc55569f252e8ff5f85";
        };
      };
      "cssom-0.3.2" = {
        name = "cssom";
        packageName = "cssom";
        version = "0.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/cssom/-/cssom-0.3.2.tgz";
          sha1 = "b8036170c79f07a90ff2f16e22284027a243848b";
        };
      };
      "cssstyle-0.2.37" = {
        name = "cssstyle";
        packageName = "cssstyle";
        version = "0.2.37";
        src = fetchurl {
          url = "https://registry.npmjs.org/cssstyle/-/cssstyle-0.2.37.tgz";
          sha1 = "541097234cb2513c83ceed3acddc27ff27987d54";
        };
      };
      "currently-unhandled-0.4.1" = {
        name = "currently-unhandled";
        packageName = "currently-unhandled";
        version = "0.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/currently-unhandled/-/currently-unhandled-0.4.1.tgz";
          sha1 = "988df33feab191ef799a61369dd76c17adf957ea";
        };
      };
      "cyclist-0.2.2" = {
        name = "cyclist";
        packageName = "cyclist";
        version = "0.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/cyclist/-/cyclist-0.2.2.tgz";
          sha1 = "1b33792e11e914a2fd6d6ed6447464444e5fa640";
        };
      };
      "d-1.0.0" = {
        name = "d";
        packageName = "d";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/d/-/d-1.0.0.tgz";
          sha1 = "754bb5bfe55451da69a58b94d45f4c5b0462d58f";
        };
      };
      "dargs-5.1.0" = {
        name = "dargs";
        packageName = "dargs";
        version = "5.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/dargs/-/dargs-5.1.0.tgz";
          sha1 = "ec7ea50c78564cd36c9d5ec18f66329fade27829";
        };
      };
      "dashdash-1.14.1" = {
        name = "dashdash";
        packageName = "dashdash";
        version = "1.14.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/dashdash/-/dashdash-1.14.1.tgz";
          sha1 = "853cfa0f7cbe2fed5de20326b8dd581035f6e2f0";
        };
      };
      "date-now-0.1.4" = {
        name = "date-now";
        packageName = "date-now";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/date-now/-/date-now-0.1.4.tgz";
          sha1 = "eaf439fd4d4848ad74e5cc7dbef200672b9e345b";
        };
      };
      "date-time-0.1.1" = {
        name = "date-time";
        packageName = "date-time";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/date-time/-/date-time-0.1.1.tgz";
          sha1 = "ed2f6d93d9790ce2fd66d5b5ff3edd5bbcbf3b07";
        };
      };
      "date-time-2.1.0" = {
        name = "date-time";
        packageName = "date-time";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/date-time/-/date-time-2.1.0.tgz";
          sha512 = "3iadbk2v1pr2v767ngj8n1pnadq3w56xbhdvn5kqg6wwqxj2kx68kfik90j4i81v461cx3220r52fqicq17xcp845s8p5pvhpiq5pzz";
        };
      };
      "debug-2.6.9" = {
        name = "debug";
        packageName = "debug";
        version = "2.6.9";
        src = fetchurl {
          url = "https://registry.npmjs.org/debug/-/debug-2.6.9.tgz";
          sha512 = "0q0fsr8bk1m83z0am0h2xn09vyfcf18adscxms8hclznwks1aihsisd96h8npx0idq5wwnypnqrkyk25m5d9zh3dk7rjs29nybc8bkc";
        };
      };
      "debug-3.1.0" = {
        name = "debug";
        packageName = "debug";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/debug/-/debug-3.1.0.tgz";
          sha512 = "3g1hqsahr1ks2kpvdxrwzr57fj90nnr0hvwwrw8yyyzcv3i11sym8zwibxx67bl1mln0acddrzpkkdjjxnc6n2cm9fazmgzzsl1fzrr";
        };
      };
      "decamelize-1.2.0" = {
        name = "decamelize";
        packageName = "decamelize";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/decamelize/-/decamelize-1.2.0.tgz";
          sha1 = "f6534d15148269b20352e7bee26f501f9a191290";
        };
      };
      "decode-uri-component-0.2.0" = {
        name = "decode-uri-component";
        packageName = "decode-uri-component";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/decode-uri-component/-/decode-uri-component-0.2.0.tgz";
          sha1 = "eb3913333458775cb84cd1a1fae062106bb87545";
        };
      };
      "deep-equal-1.0.1" = {
        name = "deep-equal";
        packageName = "deep-equal";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/deep-equal/-/deep-equal-1.0.1.tgz";
          sha1 = "f5d260292b660e084eff4cdbc9f08ad3247448b5";
        };
      };
      "deep-extend-0.4.2" = {
        name = "deep-extend";
        packageName = "deep-extend";
        version = "0.4.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/deep-extend/-/deep-extend-0.4.2.tgz";
          sha1 = "48b699c27e334bf89f10892be432f6e4c7d34a7f";
        };
      };
      "deep-is-0.1.3" = {
        name = "deep-is";
        packageName = "deep-is";
        version = "0.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz";
          sha1 = "b369d6fb5dbc13eecf524f91b070feedc357cf34";
        };
      };
      "define-properties-1.1.2" = {
        name = "define-properties";
        packageName = "define-properties";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/define-properties/-/define-properties-1.1.2.tgz";
          sha1 = "83a73f2fea569898fb737193c8f873caf6d45c94";
        };
      };
      "define-property-0.2.5" = {
        name = "define-property";
        packageName = "define-property";
        version = "0.2.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/define-property/-/define-property-0.2.5.tgz";
          sha1 = "c35b1ef918ec3c990f9a5bc57be04aacec5c8116";
        };
      };
      "define-property-1.0.0" = {
        name = "define-property";
        packageName = "define-property";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/define-property/-/define-property-1.0.0.tgz";
          sha1 = "769ebaaf3f4a63aad3af9e8d304c9bbe79bfb0e6";
        };
      };
      "define-property-2.0.2" = {
        name = "define-property";
        packageName = "define-property";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/define-property/-/define-property-2.0.2.tgz";
          sha512 = "1hrk3pzr4hyhfnrjy1msrlj2zh769x4l35q9fq0a2cy2glq3h1iwjkpmvz39y9idwy71h9hd98lydi92485bzkff1dzm70wbr8vc0lg";
        };
      };
      "defined-1.0.0" = {
        name = "defined";
        packageName = "defined";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/defined/-/defined-1.0.0.tgz";
          sha1 = "c98d9bcef75674188e110969151199e39b1fa693";
        };
      };
      "del-3.0.0" = {
        name = "del";
        packageName = "del";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/del/-/del-3.0.0.tgz";
          sha1 = "53ecf699ffcbcb39637691ab13baf160819766e5";
        };
      };
      "delayed-stream-1.0.0" = {
        name = "delayed-stream";
        packageName = "delayed-stream";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
          sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
        };
      };
      "depd-1.1.1" = {
        name = "depd";
        packageName = "depd";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/depd/-/depd-1.1.1.tgz";
          sha1 = "5783b4e1c459f06fa5ca27f991f3d06e7a310359";
        };
      };
      "depd-1.1.2" = {
        name = "depd";
        packageName = "depd";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/depd/-/depd-1.1.2.tgz";
          sha1 = "9bcd52e14c097763e749b274c4346ed2e560b5a9";
        };
      };
      "des.js-1.0.0" = {
        name = "des.js";
        packageName = "des.js";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/des.js/-/des.js-1.0.0.tgz";
          sha1 = "c074d2e2aa6a8a9a07dbd61f9a15c2cd83ec8ecc";
        };
      };
      "destroy-1.0.4" = {
        name = "destroy";
        packageName = "destroy";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/destroy/-/destroy-1.0.4.tgz";
          sha1 = "978857442c44749e4206613e37946205826abd80";
        };
      };
      "detect-indent-4.0.0" = {
        name = "detect-indent";
        packageName = "detect-indent";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/detect-indent/-/detect-indent-4.0.0.tgz";
          sha1 = "f76d064352cdf43a1cb6ce619c4ee3a9475de208";
        };
      };
      "detect-indent-5.0.0" = {
        name = "detect-indent";
        packageName = "detect-indent";
        version = "5.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/detect-indent/-/detect-indent-5.0.0.tgz";
          sha1 = "3871cc0a6a002e8c3e5b3cf7f336264675f06b9d";
        };
      };
      "detect-node-2.0.3" = {
        name = "detect-node";
        packageName = "detect-node";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/detect-node/-/detect-node-2.0.3.tgz";
          sha1 = "a2033c09cc8e158d37748fbde7507832bd6ce127";
        };
      };
      "diffie-hellman-5.0.2" = {
        name = "diffie-hellman";
        packageName = "diffie-hellman";
        version = "5.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/diffie-hellman/-/diffie-hellman-5.0.2.tgz";
          sha1 = "b5835739270cfe26acf632099fded2a07f209e5e";
        };
      };
      "dir-glob-2.0.0" = {
        name = "dir-glob";
        packageName = "dir-glob";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/dir-glob/-/dir-glob-2.0.0.tgz";
          sha512 = "1m705yfirf97v4w87gfvylhhq9jlwjsgfp5x0p0cp33mc180ldmvgbs06zmr7by48d7r01n3awx4xz3m3vzba99gqww1wgka2na5fnz";
        };
      };
      "dns-equal-1.0.0" = {
        name = "dns-equal";
        packageName = "dns-equal";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/dns-equal/-/dns-equal-1.0.0.tgz";
          sha1 = "b39e7f1da6eb0a75ba9c17324b34753c47e0654d";
        };
      };
      "dns-packet-1.3.1" = {
        name = "dns-packet";
        packageName = "dns-packet";
        version = "1.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/dns-packet/-/dns-packet-1.3.1.tgz";
          sha512 = "19g682cvkba33mwrism28hibd2nv9xd16k5bj807jx3ih1cc7ff9dn8chmfjnqgglzl6lq3m3jarxng9vbarccgchd0aq118d15yk6i";
        };
      };
      "dns-txt-2.0.2" = {
        name = "dns-txt";
        packageName = "dns-txt";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/dns-txt/-/dns-txt-2.0.2.tgz";
          sha1 = "b91d806f5d27188e4ab3e7d107d881a1cc4642b6";
        };
      };
      "dom-converter-0.1.4" = {
        name = "dom-converter";
        packageName = "dom-converter";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/dom-converter/-/dom-converter-0.1.4.tgz";
          sha1 = "a45ef5727b890c9bffe6d7c876e7b19cb0e17f3b";
        };
      };
      "dom-serializer-0.1.0" = {
        name = "dom-serializer";
        packageName = "dom-serializer";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/dom-serializer/-/dom-serializer-0.1.0.tgz";
          sha1 = "073c697546ce0780ce23be4a28e293e40bc30c82";
        };
      };
      "domain-browser-1.2.0" = {
        name = "domain-browser";
        packageName = "domain-browser";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/domain-browser/-/domain-browser-1.2.0.tgz";
          sha512 = "1fcxv8rzfhs99afvhji7bs5ppxwn9mw040ixdgvkm6iabz72q61arly2lr57086rjn4g2vkb3rkih1cyc7z35kzv1jjciwyrs4g4y4f";
        };
      };
      "domelementtype-1.1.3" = {
        name = "domelementtype";
        packageName = "domelementtype";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/domelementtype/-/domelementtype-1.1.3.tgz";
          sha1 = "bd28773e2642881aec51544924299c5cd822185b";
        };
      };
      "domelementtype-1.3.0" = {
        name = "domelementtype";
        packageName = "domelementtype";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/domelementtype/-/domelementtype-1.3.0.tgz";
          sha1 = "b17aed82e8ab59e52dd9c19b1756e0fc187204c2";
        };
      };
      "domexception-1.0.1" = {
        name = "domexception";
        packageName = "domexception";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/domexception/-/domexception-1.0.1.tgz";
          sha512 = "2x8mi3fy0wghky9i6bmz1d26yz1bbd4zmaqyc45fhrqhw8kdg6a4k4zz6jn9bisnxr8bff7fypz0vncby6sw8d2ld6r627v94ra1a5d";
        };
      };
      "domhandler-2.1.0" = {
        name = "domhandler";
        packageName = "domhandler";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/domhandler/-/domhandler-2.1.0.tgz";
          sha1 = "d2646f5e57f6c3bab11cf6cb05d3c0acf7412594";
        };
      };
      "domhandler-2.4.1" = {
        name = "domhandler";
        packageName = "domhandler";
        version = "2.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/domhandler/-/domhandler-2.4.1.tgz";
          sha1 = "892e47000a99be55bbf3774ffea0561d8879c259";
        };
      };
      "domutils-1.1.6" = {
        name = "domutils";
        packageName = "domutils";
        version = "1.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/domutils/-/domutils-1.1.6.tgz";
          sha1 = "bddc3de099b9a2efacc51c623f28f416ecc57485";
        };
      };
      "domutils-1.5.1" = {
        name = "domutils";
        packageName = "domutils";
        version = "1.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/domutils/-/domutils-1.5.1.tgz";
          sha1 = "dcd8488a26f563d61079e48c9f7b7e32373682cf";
        };
      };
      "dot-prop-4.2.0" = {
        name = "dot-prop";
        packageName = "dot-prop";
        version = "4.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/dot-prop/-/dot-prop-4.2.0.tgz";
          sha512 = "2wyv9brsq3dzp724y1q5z5j5ja83y834hgc193lnarfdycwz1ii3xg02qxx3dg05x3skwjm1di5s5a8hqi8l5v1afx2bia436pifhxm";
        };
      };
      "duplexer3-0.1.4" = {
        name = "duplexer3";
        packageName = "duplexer3";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/duplexer3/-/duplexer3-0.1.4.tgz";
          sha1 = "ee01dd1cac0ed3cbc7fdbea37dc0a8f1ce002ce2";
        };
      };
      "duplexify-3.5.4" = {
        name = "duplexify";
        packageName = "duplexify";
        version = "3.5.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/duplexify/-/duplexify-3.5.4.tgz";
          sha512 = "2qcky919ps17a9ndimxvcqc73wlrcjmq8ppddbnl45xs9yqp1dmzzfaspfn63xzp14rl3dlk4g6y2ia71s6r9nggd0mb891hcni4di7";
        };
      };
      "ecc-jsbn-0.1.1" = {
        name = "ecc-jsbn";
        packageName = "ecc-jsbn";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.1.tgz";
          sha1 = "0fc73a9ed5f0d53c38193398523ef7e543777505";
        };
      };
      "ee-first-1.1.1" = {
        name = "ee-first";
        packageName = "ee-first";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz";
          sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
        };
      };
      "electron-to-chromium-1.3.36" = {
        name = "electron-to-chromium";
        packageName = "electron-to-chromium";
        version = "1.3.36";
        src = fetchurl {
          url = "https://registry.npmjs.org/electron-to-chromium/-/electron-to-chromium-1.3.36.tgz";
          sha1 = "0eabf71a9ebea9013fb1cc35a390e068624f27e8";
        };
      };
      "elliptic-6.4.0" = {
        name = "elliptic";
        packageName = "elliptic";
        version = "6.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/elliptic/-/elliptic-6.4.0.tgz";
          sha1 = "cac9af8762c85836187003c8dfe193e5e2eae5df";
        };
      };
      "emojis-list-2.1.0" = {
        name = "emojis-list";
        packageName = "emojis-list";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/emojis-list/-/emojis-list-2.1.0.tgz";
          sha1 = "4daa4d9db00f9819880c79fa457ae5b09a1fd389";
        };
      };
      "empower-core-0.6.2" = {
        name = "empower-core";
        packageName = "empower-core";
        version = "0.6.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/empower-core/-/empower-core-0.6.2.tgz";
          sha1 = "5adef566088e31fba80ba0a36df47d7094169144";
        };
      };
      "encodeurl-1.0.2" = {
        name = "encodeurl";
        packageName = "encodeurl";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.2.tgz";
          sha1 = "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59";
        };
      };
      "encoding-0.1.12" = {
        name = "encoding";
        packageName = "encoding";
        version = "0.1.12";
        src = fetchurl {
          url = "https://registry.npmjs.org/encoding/-/encoding-0.1.12.tgz";
          sha1 = "538b66f3ee62cd1ab51ec323829d1f9480c74beb";
        };
      };
      "end-of-stream-1.4.1" = {
        name = "end-of-stream";
        packageName = "end-of-stream";
        version = "1.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.4.1.tgz";
          sha512 = "3cjrpi6n5i6gf8jaiwg31y2xkgx59szhhcj9myqwmdw16s9r6yvwznxd2lhqf96mpm6knyb3w2bcnksg5nzkrq6iada0k6nvdj2pjfl";
        };
      };
      "engine.io-client-3.1.6" = {
        name = "engine.io-client";
        packageName = "engine.io-client";
        version = "3.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/engine.io-client/-/engine.io-client-3.1.6.tgz";
          sha512 = "1zfr0lycsgvwkxc8i4qhg5rjr59ss7p66nz3j1y1awmvzgy9myn0rqrpv2hrnm76qv9kmiiss0jpxaaz9xy0lkp7857npkfb6q8fyw6";
        };
      };
      "engine.io-parser-2.1.2" = {
        name = "engine.io-parser";
        packageName = "engine.io-parser";
        version = "2.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/engine.io-parser/-/engine.io-parser-2.1.2.tgz";
          sha512 = "0rjbixsn5qvjwklnvvjdfz4wy85dk82zkvh6lk3znbd3p3isgr57a5kikgndr3xhhkv5zzvh4bfxbz7gqfsgijscyiiilgw78bwp2bl";
        };
      };
      "enhanced-resolve-3.4.1" = {
        name = "enhanced-resolve";
        packageName = "enhanced-resolve";
        version = "3.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/enhanced-resolve/-/enhanced-resolve-3.4.1.tgz";
          sha1 = "0421e339fd71419b3da13d129b3979040230476e";
        };
      };
      "entities-1.1.1" = {
        name = "entities";
        packageName = "entities";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/entities/-/entities-1.1.1.tgz";
          sha1 = "6e5c2d0a5621b5dadaecef80b90edfb5cd7772f0";
        };
      };
      "equal-length-1.0.1" = {
        name = "equal-length";
        packageName = "equal-length";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/equal-length/-/equal-length-1.0.1.tgz";
          sha1 = "21ca112d48ab24b4e1e7ffc0e5339d31fdfc274c";
        };
      };
      "err-code-1.1.2" = {
        name = "err-code";
        packageName = "err-code";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/err-code/-/err-code-1.1.2.tgz";
          sha1 = "06e0116d3028f6aef4806849eb0ea6a748ae6960";
        };
      };
      "errno-0.1.7" = {
        name = "errno";
        packageName = "errno";
        version = "0.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/errno/-/errno-0.1.7.tgz";
          sha512 = "2bdzcjwgdkg5yrvlw6my57pn77k4j7a2pzppwqrq4va9f5bd4b5mzbhwpklhsy1jl7w9sjvnfs30h42nhz2dbdfhagnh8dk6l2d3yii";
        };
      };
      "error-ex-1.3.1" = {
        name = "error-ex";
        packageName = "error-ex";
        version = "1.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/error-ex/-/error-ex-1.3.1.tgz";
          sha1 = "f855a86ce61adc4e8621c3cda21e7a7612c3a8dc";
        };
      };
      "es-abstract-1.10.0" = {
        name = "es-abstract";
        packageName = "es-abstract";
        version = "1.10.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/es-abstract/-/es-abstract-1.10.0.tgz";
          sha512 = "04nd5ylkfffc08vn5kjhz0saqh44nj19f5j3ahrrhf3zvc9da5rf6snnh63xv4gfhacjbax1jajzgqv4zpm77v806jf883a2w77zs7y";
        };
      };
      "es-to-primitive-1.1.1" = {
        name = "es-to-primitive";
        packageName = "es-to-primitive";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/es-to-primitive/-/es-to-primitive-1.1.1.tgz";
          sha1 = "45355248a88979034b6792e19bb81f2b7975dd0d";
        };
      };
      "es5-ext-0.10.40" = {
        name = "es5-ext";
        packageName = "es5-ext";
        version = "0.10.40";
        src = fetchurl {
          url = "https://registry.npmjs.org/es5-ext/-/es5-ext-0.10.40.tgz";
          sha512 = "2ylf5xas3zi45vg84c8r81d1lw1d1wmckzf8pvnkwgmq6js0n5mms6lrpnmac7sbi4fv9aqlnrspz7s97vbqii3c6py7r4sikg63lab";
        };
      };
      "es6-error-4.1.1" = {
        name = "es6-error";
        packageName = "es6-error";
        version = "4.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-error/-/es6-error-4.1.1.tgz";
          sha512 = "1b98y4j9fy6c2wm7ys3csnyfg8cn40sy2g958i45fdh5bnx1lkl19d4508aldabga5rm1q5hzxq68yjdyb8n6qxb8925x1b2cbzwvsj";
        };
      };
      "es6-iterator-2.0.3" = {
        name = "es6-iterator";
        packageName = "es6-iterator";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-iterator/-/es6-iterator-2.0.3.tgz";
          sha1 = "a7de889141a05a94b0854403b2d0a0fbfa98f3b7";
        };
      };
      "es6-map-0.1.5" = {
        name = "es6-map";
        packageName = "es6-map";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-map/-/es6-map-0.1.5.tgz";
          sha1 = "9136e0503dcc06a301690f0bb14ff4e364e949f0";
        };
      };
      "es6-set-0.1.5" = {
        name = "es6-set";
        packageName = "es6-set";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-set/-/es6-set-0.1.5.tgz";
          sha1 = "d2b3ec5d4d800ced818db538d28974db0a73ccb1";
        };
      };
      "es6-symbol-3.1.1" = {
        name = "es6-symbol";
        packageName = "es6-symbol";
        version = "3.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-symbol/-/es6-symbol-3.1.1.tgz";
          sha1 = "bf00ef4fdab6ba1b46ecb7b629b4c7ed5715cc77";
        };
      };
      "es6-weak-map-2.0.2" = {
        name = "es6-weak-map";
        packageName = "es6-weak-map";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/es6-weak-map/-/es6-weak-map-2.0.2.tgz";
          sha1 = "5e3ab32251ffd1538a1f8e5ffa1357772f92d96f";
        };
      };
      "escape-html-1.0.3" = {
        name = "escape-html";
        packageName = "escape-html";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz";
          sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
        };
      };
      "escape-string-regexp-1.0.5" = {
        name = "escape-string-regexp";
        packageName = "escape-string-regexp";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
          sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
        };
      };
      "escodegen-1.9.1" = {
        name = "escodegen";
        packageName = "escodegen";
        version = "1.9.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/escodegen/-/escodegen-1.9.1.tgz";
          sha512 = "3nl4jbvigqc5ax1f35b601vlca4fa4ghr27wfglgrmv7h5pssnkh7px0vjh8wzrkjvx8bw7c71b0axc307akkjd1vkl2nj0acxy657a";
        };
      };
      "escope-3.6.0" = {
        name = "escope";
        packageName = "escope";
        version = "3.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/escope/-/escope-3.6.0.tgz";
          sha1 = "e01975e812781a163a6dadfdd80398dc64c889c3";
        };
      };
      "espower-location-detector-1.0.0" = {
        name = "espower-location-detector";
        packageName = "espower-location-detector";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/espower-location-detector/-/espower-location-detector-1.0.0.tgz";
          sha1 = "a17b7ecc59d30e179e2bef73fb4137704cb331b5";
        };
      };
      "esprima-2.7.3" = {
        name = "esprima";
        packageName = "esprima";
        version = "2.7.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/esprima/-/esprima-2.7.3.tgz";
          sha1 = "96e3b70d5779f6ad49cd032673d1c312767ba581";
        };
      };
      "esprima-3.1.3" = {
        name = "esprima";
        packageName = "esprima";
        version = "3.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/esprima/-/esprima-3.1.3.tgz";
          sha1 = "fdca51cee6133895e3c88d535ce49dbff62a4633";
        };
      };
      "esprima-4.0.0" = {
        name = "esprima";
        packageName = "esprima";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/esprima/-/esprima-4.0.0.tgz";
          sha512 = "27mkhd94y9vrr8pb97br0ym5h85rawwb0biswgwdfp31x0387y12k9p9598bi4fc83fif6crfzqiqmmjs4x7gcb22ml3z1fldqm7yx1";
        };
      };
      "espurify-1.7.0" = {
        name = "espurify";
        packageName = "espurify";
        version = "1.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/espurify/-/espurify-1.7.0.tgz";
          sha1 = "1c5cf6cbccc32e6f639380bd4f991fab9ba9d226";
        };
      };
      "esrecurse-4.2.1" = {
        name = "esrecurse";
        packageName = "esrecurse";
        version = "4.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/esrecurse/-/esrecurse-4.2.1.tgz";
          sha512 = "06zyknk70y80n428x20wvjqy5822wgz2yaxzw387njnz3ar6ixzjcxzr2jsl8wrm0m9jpa4ysbcr9znpk6gbkqd7wby084zxw3l317b";
        };
      };
      "estraverse-4.2.0" = {
        name = "estraverse";
        packageName = "estraverse";
        version = "4.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/estraverse/-/estraverse-4.2.0.tgz";
          sha1 = "0dee3fed31fcd469618ce7342099fc1afa0bdb13";
        };
      };
      "esutils-2.0.2" = {
        name = "esutils";
        packageName = "esutils";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/esutils/-/esutils-2.0.2.tgz";
          sha1 = "0abf4f1caa5bcb1f7a9d8acc6dea4faaa04bac9b";
        };
      };
      "etag-1.8.1" = {
        name = "etag";
        packageName = "etag";
        version = "1.8.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz";
          sha1 = "41ae2eeb65efa62268aebfea83ac7d79299b0887";
        };
      };
      "event-emitter-0.3.5" = {
        name = "event-emitter";
        packageName = "event-emitter";
        version = "0.3.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/event-emitter/-/event-emitter-0.3.5.tgz";
          sha1 = "df8c69eef1647923c7157b9ce83840610b02cc39";
        };
      };
      "eventemitter3-1.2.0" = {
        name = "eventemitter3";
        packageName = "eventemitter3";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/eventemitter3/-/eventemitter3-1.2.0.tgz";
          sha1 = "1c86991d816ad1e504750e73874224ecf3bec508";
        };
      };
      "events-1.1.1" = {
        name = "events";
        packageName = "events";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/events/-/events-1.1.1.tgz";
          sha1 = "9ebdb7635ad099c70dcc4c2a1f5004288e8bd924";
        };
      };
      "eventsource-0.1.6" = {
        name = "eventsource";
        packageName = "eventsource";
        version = "0.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/eventsource/-/eventsource-0.1.6.tgz";
          sha1 = "0acede849ed7dd1ccc32c811bb11b944d4f29232";
        };
      };
      "evp_bytestokey-1.0.3" = {
        name = "evp_bytestokey";
        packageName = "evp_bytestokey";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/evp_bytestokey/-/evp_bytestokey-1.0.3.tgz";
          sha512 = "1wd18zxd7n42asa63aa4k1bdf58warg29c7c8cdzzkd4r1wva7qwzqnn52h8g8hqwj7bxjkk3ryghajrvz4i27h5bzp30p8hjiqdzgx";
        };
      };
      "execa-0.7.0" = {
        name = "execa";
        packageName = "execa";
        version = "0.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/execa/-/execa-0.7.0.tgz";
          sha1 = "944becd34cc41ee32a63a9faf27ad5a65fc59777";
        };
      };
      "expand-brackets-0.1.5" = {
        name = "expand-brackets";
        packageName = "expand-brackets";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/expand-brackets/-/expand-brackets-0.1.5.tgz";
          sha1 = "df07284e342a807cd733ac5af72411e581d1177b";
        };
      };
      "expand-brackets-2.1.4" = {
        name = "expand-brackets";
        packageName = "expand-brackets";
        version = "2.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/expand-brackets/-/expand-brackets-2.1.4.tgz";
          sha1 = "b77735e315ce30f6b6eff0f83b04151a22449622";
        };
      };
      "expand-range-1.8.2" = {
        name = "expand-range";
        packageName = "expand-range";
        version = "1.8.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/expand-range/-/expand-range-1.8.2.tgz";
          sha1 = "a299effd335fe2721ebae8e257ec79644fc85337";
        };
      };
      "express-4.16.2" = {
        name = "express";
        packageName = "express";
        version = "4.16.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/express/-/express-4.16.2.tgz";
          sha1 = "e35c6dfe2d64b7dca0a5cd4f21781be3299e076c";
        };
      };
      "extend-3.0.1" = {
        name = "extend";
        packageName = "extend";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/extend/-/extend-3.0.1.tgz";
          sha1 = "a755ea7bc1adfcc5a31ce7e762dbaadc5e636444";
        };
      };
      "extend-shallow-2.0.1" = {
        name = "extend-shallow";
        packageName = "extend-shallow";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz";
          sha1 = "51af7d614ad9a9f610ea1bafbb989d6b1c56890f";
        };
      };
      "extend-shallow-3.0.2" = {
        name = "extend-shallow";
        packageName = "extend-shallow";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-3.0.2.tgz";
          sha1 = "26a71aaf073b39fb2127172746131c2704028db8";
        };
      };
      "extglob-0.3.2" = {
        name = "extglob";
        packageName = "extglob";
        version = "0.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/extglob/-/extglob-0.3.2.tgz";
          sha1 = "2e18ff3d2f49ab2765cec9023f011daa8d8349a1";
        };
      };
      "extglob-2.0.4" = {
        name = "extglob";
        packageName = "extglob";
        version = "2.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/extglob/-/extglob-2.0.4.tgz";
          sha512 = "2klp0045k4wnaspb9khqx90ddv7rjg997mlyp5qz41sl2yqdrpw8g8wji77qq16aawl4yhvg0f993ln48lja0kfmy0wnbh4g50zlrin";
        };
      };
      "extract-text-webpack-plugin-3.0.2" = {
        name = "extract-text-webpack-plugin";
        packageName = "extract-text-webpack-plugin";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/extract-text-webpack-plugin/-/extract-text-webpack-plugin-3.0.2.tgz";
          sha512 = "10rc4hfyzf8n5b30q0fpawnbnr1cs33mmrz9466hhcxjzlz7ys44zbsr5kbvr1jn5nivcr91nmfhpq2mqi1wxmr19zsnimri5kwppvf";
        };
      };
      "extsprintf-1.3.0" = {
        name = "extsprintf";
        packageName = "extsprintf";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/extsprintf/-/extsprintf-1.3.0.tgz";
          sha1 = "96918440e3041a7a414f8c52e3c574eb3c3e1e05";
        };
      };
      "fast-deep-equal-1.1.0" = {
        name = "fast-deep-equal";
        packageName = "fast-deep-equal";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-1.1.0.tgz";
          sha1 = "c053477817c86b51daa853c81e059b733d023614";
        };
      };
      "fast-diff-1.1.2" = {
        name = "fast-diff";
        packageName = "fast-diff";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/fast-diff/-/fast-diff-1.1.2.tgz";
          sha512 = "2550z1qvyfv9js9vg2aaj57ji5d9hhg4f6zl4rf483d6xswv23ac6ipj8gbapv4sjx14dpcslqmnx1z78vvx4np4ad5mdrxwfvm98i9";
        };
      };
      "fast-json-stable-stringify-2.0.0" = {
        name = "fast-json-stable-stringify";
        packageName = "fast-json-stable-stringify";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.0.0.tgz";
          sha1 = "d5142c0caee6b1189f87d3a76111064f86c8bbf2";
        };
      };
      "fast-levenshtein-2.0.6" = {
        name = "fast-levenshtein";
        packageName = "fast-levenshtein";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz";
          sha1 = "3d8a5c66883a16a30ca8643e851f19baa7797917";
        };
      };
      "fastparse-1.1.1" = {
        name = "fastparse";
        packageName = "fastparse";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/fastparse/-/fastparse-1.1.1.tgz";
          sha1 = "d1e2643b38a94d7583b479060e6c4affc94071f8";
        };
      };
      "faye-websocket-0.10.0" = {
        name = "faye-websocket";
        packageName = "faye-websocket";
        version = "0.10.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/faye-websocket/-/faye-websocket-0.10.0.tgz";
          sha1 = "4e492f8d04dfb6f89003507f6edbf2d501e7c6f4";
        };
      };
      "faye-websocket-0.11.1" = {
        name = "faye-websocket";
        packageName = "faye-websocket";
        version = "0.11.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/faye-websocket/-/faye-websocket-0.11.1.tgz";
          sha1 = "f0efe18c4f56e4f40afc7e06c719fd5ee6188f38";
        };
      };
      "fbjs-0.8.16" = {
        name = "fbjs";
        packageName = "fbjs";
        version = "0.8.16";
        src = fetchurl {
          url = "https://registry.npmjs.org/fbjs/-/fbjs-0.8.16.tgz";
          sha1 = "5e67432f550dc41b572bf55847b8aca64e5337db";
        };
      };
      "figures-2.0.0" = {
        name = "figures";
        packageName = "figures";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/figures/-/figures-2.0.0.tgz";
          sha1 = "3ab1a2d2a62c8bfb431a0c94cb797a2fce27c962";
        };
      };
      "file-loader-1.1.11" = {
        name = "file-loader";
        packageName = "file-loader";
        version = "1.1.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/file-loader/-/file-loader-1.1.11.tgz";
          sha512 = "0p00kmxlfgsw0mn4y5h5njv0g1y7gakpygdv6wz318wzpll8zd44r7q0fvym4v837054n04d22r2klkqnckr3k0x2hc2ln79qfphr2c";
        };
      };
      "filename-regex-2.0.1" = {
        name = "filename-regex";
        packageName = "filename-regex";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/filename-regex/-/filename-regex-2.0.1.tgz";
          sha1 = "c1c4b9bee3e09725ddb106b75c1e301fe2f18b26";
        };
      };
      "fill-range-2.2.3" = {
        name = "fill-range";
        packageName = "fill-range";
        version = "2.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/fill-range/-/fill-range-2.2.3.tgz";
          sha1 = "50b77dfd7e469bc7492470963699fe7a8485a723";
        };
      };
      "fill-range-4.0.0" = {
        name = "fill-range";
        packageName = "fill-range";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/fill-range/-/fill-range-4.0.0.tgz";
          sha1 = "d544811d428f98eb06a63dc402d2403c328c38f7";
        };
      };
      "finalhandler-1.1.0" = {
        name = "finalhandler";
        packageName = "finalhandler";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/finalhandler/-/finalhandler-1.1.0.tgz";
          sha1 = "ce0b6855b45853e791b2fcc680046d88253dd7f5";
        };
      };
      "find-cache-dir-0.1.1" = {
        name = "find-cache-dir";
        packageName = "find-cache-dir";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/find-cache-dir/-/find-cache-dir-0.1.1.tgz";
          sha1 = "c8defae57c8a52a8a784f9e31c57c742e993a0b9";
        };
      };
      "find-cache-dir-1.0.0" = {
        name = "find-cache-dir";
        packageName = "find-cache-dir";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/find-cache-dir/-/find-cache-dir-1.0.0.tgz";
          sha1 = "9288e3e9e3cc3748717d39eade17cf71fc30ee6f";
        };
      };
      "find-up-1.1.2" = {
        name = "find-up";
        packageName = "find-up";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/find-up/-/find-up-1.1.2.tgz";
          sha1 = "6b2e9822b1a2ce0a60ab64d610eccad53cb24d0f";
        };
      };
      "find-up-2.1.0" = {
        name = "find-up";
        packageName = "find-up";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/find-up/-/find-up-2.1.0.tgz";
          sha1 = "45d1b7e506c717ddd482775a2b77920a3c0c57a7";
        };
      };
      "flatten-1.0.2" = {
        name = "flatten";
        packageName = "flatten";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/flatten/-/flatten-1.0.2.tgz";
          sha1 = "dae46a9d78fbe25292258cc1e780a41d95c03782";
        };
      };
      "flush-write-stream-1.0.2" = {
        name = "flush-write-stream";
        packageName = "flush-write-stream";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/flush-write-stream/-/flush-write-stream-1.0.2.tgz";
          sha1 = "c81b90d8746766f1a609a46809946c45dd8ae417";
        };
      };
      "fn-name-2.0.1" = {
        name = "fn-name";
        packageName = "fn-name";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/fn-name/-/fn-name-2.0.1.tgz";
          sha1 = "5214d7537a4d06a4a301c0cc262feb84188002e7";
        };
      };
      "for-in-1.0.2" = {
        name = "for-in";
        packageName = "for-in";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/for-in/-/for-in-1.0.2.tgz";
          sha1 = "81068d295a8142ec0ac726c6e2200c30fb6d5e80";
        };
      };
      "for-own-0.1.5" = {
        name = "for-own";
        packageName = "for-own";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/for-own/-/for-own-0.1.5.tgz";
          sha1 = "5265c681a4f294dabbf17c9509b6763aa84510ce";
        };
      };
      "foreach-2.0.5" = {
        name = "foreach";
        packageName = "foreach";
        version = "2.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/foreach/-/foreach-2.0.5.tgz";
          sha1 = "0bee005018aeb260d0a3af3ae658dd0136ec1b99";
        };
      };
      "forever-agent-0.6.1" = {
        name = "forever-agent";
        packageName = "forever-agent";
        version = "0.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz";
          sha1 = "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91";
        };
      };
      "form-data-2.3.2" = {
        name = "form-data";
        packageName = "form-data";
        version = "2.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/form-data/-/form-data-2.3.2.tgz";
          sha1 = "4970498be604c20c005d4f5c23aecd21d6b49099";
        };
      };
      "forwarded-0.1.2" = {
        name = "forwarded";
        packageName = "forwarded";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/forwarded/-/forwarded-0.1.2.tgz";
          sha1 = "98c23dab1175657b8c0573e8ceccd91b0ff18c84";
        };
      };
      "fragment-cache-0.2.1" = {
        name = "fragment-cache";
        packageName = "fragment-cache";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/fragment-cache/-/fragment-cache-0.2.1.tgz";
          sha1 = "4290fad27f13e89be7f33799c6bc5a0abfff0d19";
        };
      };
      "fresh-0.5.2" = {
        name = "fresh";
        packageName = "fresh";
        version = "0.5.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/fresh/-/fresh-0.5.2.tgz";
          sha1 = "3d8cadd90d976569fa835ab1f8e4b23a105605a7";
        };
      };
      "from2-2.3.0" = {
        name = "from2";
        packageName = "from2";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/from2/-/from2-2.3.0.tgz";
          sha1 = "8bfb5502bde4a4d36cfdeea007fcca21d7e382af";
        };
      };
      "fs-write-stream-atomic-1.0.10" = {
        name = "fs-write-stream-atomic";
        packageName = "fs-write-stream-atomic";
        version = "1.0.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/fs-write-stream-atomic/-/fs-write-stream-atomic-1.0.10.tgz";
          sha1 = "b47df53493ef911df75731e70a9ded0189db40c9";
        };
      };
      "fs.realpath-1.0.0" = {
        name = "fs.realpath";
        packageName = "fs.realpath";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
          sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
        };
      };
      "fsevents-1.1.3" = {
        name = "fsevents";
        packageName = "fsevents";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/fsevents/-/fsevents-1.1.3.tgz";
          sha512 = "3jw51f4iayxvp9wfxczk1xgcvhsydhlgah64jmpl0mqiii2h8i5pp0lrqac5xn7296gxqrvy4lgm4k4hkifk8gipgqxd68x764gp2jq";
        };
      };
      "function-bind-1.1.1" = {
        name = "function-bind";
        packageName = "function-bind";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz";
          sha512 = "38chm1mh077ksx6hy2sssfz4q29hf0ncb9k6ila7si54zqcpl5fxd1rh6wi82blqp7jcspf4aynr7jqhbsg2yc9y42xpqqp6c1jz2n8";
        };
      };
      "function-name-support-0.2.0" = {
        name = "function-name-support";
        packageName = "function-name-support";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/function-name-support/-/function-name-support-0.2.0.tgz";
          sha1 = "55d3bfaa6eafd505a50f9bc81fdf57564a0bb071";
        };
      };
      "get-caller-file-1.0.2" = {
        name = "get-caller-file";
        packageName = "get-caller-file";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-1.0.2.tgz";
          sha1 = "f702e63127e7e231c160a80c1554acb70d5047e5";
        };
      };
      "get-port-3.2.0" = {
        name = "get-port";
        packageName = "get-port";
        version = "3.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/get-port/-/get-port-3.2.0.tgz";
          sha1 = "dd7ce7de187c06c8bf353796ac71e099f0980ebc";
        };
      };
      "get-stdin-4.0.1" = {
        name = "get-stdin";
        packageName = "get-stdin";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/get-stdin/-/get-stdin-4.0.1.tgz";
          sha1 = "b968c6b0a04384324902e8bf1a5df32579a450fe";
        };
      };
      "get-stream-3.0.0" = {
        name = "get-stream";
        packageName = "get-stream";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/get-stream/-/get-stream-3.0.0.tgz";
          sha1 = "8e943d1358dc37555054ecbe2edb05aa174ede14";
        };
      };
      "get-value-2.0.6" = {
        name = "get-value";
        packageName = "get-value";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/get-value/-/get-value-2.0.6.tgz";
          sha1 = "dc15ca1c672387ca76bd37ac0a395ba2042a2c28";
        };
      };
      "getpass-0.1.7" = {
        name = "getpass";
        packageName = "getpass";
        version = "0.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/getpass/-/getpass-0.1.7.tgz";
          sha1 = "5eff8e3e684d569ae4cb2b1282604e8ba62149fa";
        };
      };
      "git-revision-webpack-plugin-2.5.1" = {
        name = "git-revision-webpack-plugin";
        packageName = "git-revision-webpack-plugin";
        version = "2.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/git-revision-webpack-plugin/-/git-revision-webpack-plugin-2.5.1.tgz";
          sha1 = "3fb4398f376cf27678d6de96ba2669b518645e78";
        };
      };
      "glob-6.0.4" = {
        name = "glob";
        packageName = "glob";
        version = "6.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/glob/-/glob-6.0.4.tgz";
          sha1 = "0f08860f6a155127b2fadd4f9ce24b1aab6e4d22";
        };
      };
      "glob-7.1.2" = {
        name = "glob";
        packageName = "glob";
        version = "7.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/glob/-/glob-7.1.2.tgz";
          sha512 = "08vjxzixc9dwc1hn5pd60yyij98krk2pr758aiga97r02ncvaqx1hidi95wk470k1v84gg4alls9bm52m77174z128bgf13b61x951h";
        };
      };
      "glob-base-0.3.0" = {
        name = "glob-base";
        packageName = "glob-base";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/glob-base/-/glob-base-0.3.0.tgz";
          sha1 = "dbb164f6221b1c0b1ccf82aea328b497df0ea3c4";
        };
      };
      "glob-parent-2.0.0" = {
        name = "glob-parent";
        packageName = "glob-parent";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/glob-parent/-/glob-parent-2.0.0.tgz";
          sha1 = "81383d72db054fcccf5336daa902f182f6edbb28";
        };
      };
      "glob-parent-3.1.0" = {
        name = "glob-parent";
        packageName = "glob-parent";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/glob-parent/-/glob-parent-3.1.0.tgz";
          sha1 = "9e6af6299d8d3bd2bd40430832bd113df906c5ae";
        };
      };
      "global-dirs-0.1.1" = {
        name = "global-dirs";
        packageName = "global-dirs";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/global-dirs/-/global-dirs-0.1.1.tgz";
          sha1 = "b319c0dd4607f353f3be9cca4c72fc148c49f445";
        };
      };
      "globals-9.18.0" = {
        name = "globals";
        packageName = "globals";
        version = "9.18.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/globals/-/globals-9.18.0.tgz";
          sha512 = "18psd5ig23apaw07k4mma3z1hi2ndfwsqkm05hxashnf5lf7mpfs6kjiircc0x3x3q15j2x2j4zfzsqacxvfsmw40zjchn44bfccjab";
        };
      };
      "globby-4.1.0" = {
        name = "globby";
        packageName = "globby";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/globby/-/globby-4.1.0.tgz";
          sha1 = "080f54549ec1b82a6c60e631fc82e1211dbe95f8";
        };
      };
      "globby-6.1.0" = {
        name = "globby";
        packageName = "globby";
        version = "6.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/globby/-/globby-6.1.0.tgz";
          sha1 = "f5a6d70e8395e21c858fb0489d64df02424d506c";
        };
      };
      "globby-7.1.1" = {
        name = "globby";
        packageName = "globby";
        version = "7.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/globby/-/globby-7.1.1.tgz";
          sha1 = "fb2ccff9401f8600945dfada97440cca972b8680";
        };
      };
      "got-6.7.1" = {
        name = "got";
        packageName = "got";
        version = "6.7.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/got/-/got-6.7.1.tgz";
          sha1 = "240cd05785a9a18e561dc1b44b41c763ef1e8db0";
        };
      };
      "graceful-fs-4.1.11" = {
        name = "graceful-fs";
        packageName = "graceful-fs";
        version = "4.1.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.1.11.tgz";
          sha1 = "0e8bdfe4d1ddb8854d64e04ea7c00e2a026e5658";
        };
      };
      "handle-thing-1.2.5" = {
        name = "handle-thing";
        packageName = "handle-thing";
        version = "1.2.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/handle-thing/-/handle-thing-1.2.5.tgz";
          sha1 = "fd7aad726bf1a5fd16dfc29b2f7a6601d27139c4";
        };
      };
      "har-schema-2.0.0" = {
        name = "har-schema";
        packageName = "har-schema";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/har-schema/-/har-schema-2.0.0.tgz";
          sha1 = "a94c2224ebcac04782a0d9035521f24735b7ec92";
        };
      };
      "har-validator-5.0.3" = {
        name = "har-validator";
        packageName = "har-validator";
        version = "5.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/har-validator/-/har-validator-5.0.3.tgz";
          sha1 = "ba402c266194f15956ef15e0fcf242993f6a7dfd";
        };
      };
      "has-1.0.1" = {
        name = "has";
        packageName = "has";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/has/-/has-1.0.1.tgz";
          sha1 = "8461733f538b0837c9361e39a9ab9e9704dc2f28";
        };
      };
      "has-ansi-2.0.0" = {
        name = "has-ansi";
        packageName = "has-ansi";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-ansi/-/has-ansi-2.0.0.tgz";
          sha1 = "34f5049ce1ecdf2b0649af3ef24e45ed35416d91";
        };
      };
      "has-binary2-1.0.2" = {
        name = "has-binary2";
        packageName = "has-binary2";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-binary2/-/has-binary2-1.0.2.tgz";
          sha1 = "e83dba49f0b9be4d026d27365350d9f03f54be98";
        };
      };
      "has-color-0.1.7" = {
        name = "has-color";
        packageName = "has-color";
        version = "0.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-color/-/has-color-0.1.7.tgz";
          sha1 = "67144a5260c34fc3cca677d041daf52fe7b78b2f";
        };
      };
      "has-cors-1.1.0" = {
        name = "has-cors";
        packageName = "has-cors";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-cors/-/has-cors-1.1.0.tgz";
          sha1 = "5e474793f7ea9843d1bb99c23eef49ff126fff39";
        };
      };
      "has-flag-1.0.0" = {
        name = "has-flag";
        packageName = "has-flag";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-flag/-/has-flag-1.0.0.tgz";
          sha1 = "9d9e793165ce017a00f00418c43f942a7b1d11fa";
        };
      };
      "has-flag-2.0.0" = {
        name = "has-flag";
        packageName = "has-flag";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-flag/-/has-flag-2.0.0.tgz";
          sha1 = "e8207af1cc7b30d446cc70b734b5e8be18f88d51";
        };
      };
      "has-flag-3.0.0" = {
        name = "has-flag";
        packageName = "has-flag";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz";
          sha1 = "b5d454dc2199ae225699f3467e5a07f3b955bafd";
        };
      };
      "has-value-0.3.1" = {
        name = "has-value";
        packageName = "has-value";
        version = "0.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-value/-/has-value-0.3.1.tgz";
          sha1 = "7b1f58bada62ca827ec0a2078025654845995e1f";
        };
      };
      "has-value-1.0.0" = {
        name = "has-value";
        packageName = "has-value";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-value/-/has-value-1.0.0.tgz";
          sha1 = "18b281da585b1c5c51def24c930ed29a0be6b177";
        };
      };
      "has-values-0.1.4" = {
        name = "has-values";
        packageName = "has-values";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-values/-/has-values-0.1.4.tgz";
          sha1 = "6d61de95d91dfca9b9a02089ad384bff8f62b771";
        };
      };
      "has-values-1.0.0" = {
        name = "has-values";
        packageName = "has-values";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-values/-/has-values-1.0.0.tgz";
          sha1 = "95b0b63fec2146619a6fe57fe75628d5a39efe4f";
        };
      };
      "has-yarn-1.0.0" = {
        name = "has-yarn";
        packageName = "has-yarn";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/has-yarn/-/has-yarn-1.0.0.tgz";
          sha1 = "89e25db604b725c8f5976fff0addc921b828a5a7";
        };
      };
      "hash-base-2.0.2" = {
        name = "hash-base";
        packageName = "hash-base";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/hash-base/-/hash-base-2.0.2.tgz";
          sha1 = "66ea1d856db4e8a5470cadf6fce23ae5244ef2e1";
        };
      };
      "hash-base-3.0.4" = {
        name = "hash-base";
        packageName = "hash-base";
        version = "3.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/hash-base/-/hash-base-3.0.4.tgz";
          sha1 = "5fc8686847ecd73499403319a6b0a3f3f6ae4918";
        };
      };
      "hash.js-1.1.3" = {
        name = "hash.js";
        packageName = "hash.js";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/hash.js/-/hash.js-1.1.3.tgz";
          sha512 = "0f88i7rv3ib8lwdh5z5lwrml404frzb1a9n3g25y85jpfng82vzsv7m3c5fbyrpq5ki4c3pa8823z3s61xfigm45q469nqnzp416hgx";
        };
      };
      "hawk-6.0.2" = {
        name = "hawk";
        packageName = "hawk";
        version = "6.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/hawk/-/hawk-6.0.2.tgz";
          sha512 = "1nl2hjr2mnhj5jlaz8mh54z7acwz5j5idkch04qgjk78756gw5d0fjk4a2immil5ij9ijdssb9ndpryvnh2xpcbgcjv8lxybn330als";
        };
      };
      "he-1.1.1" = {
        name = "he";
        packageName = "he";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/he/-/he-1.1.1.tgz";
          sha1 = "93410fd21b009735151f8868c2f271f3427e23fd";
        };
      };
      "hmac-drbg-1.0.1" = {
        name = "hmac-drbg";
        packageName = "hmac-drbg";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/hmac-drbg/-/hmac-drbg-1.0.1.tgz";
          sha1 = "d2745701025a6c775a6c545793ed502fc0c649a1";
        };
      };
      "hoek-4.2.1" = {
        name = "hoek";
        packageName = "hoek";
        version = "4.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/hoek/-/hoek-4.2.1.tgz";
          sha512 = "1y8kprb3qldxqj31zai5n8dvhydsl9nn5w4rskhnbzzhldn6pm6n5lcyam3sfkb61a62d5m58k8im7z6ngwbd9cw9zp4zm4y7ckrf20";
        };
      };
      "home-or-tmp-2.0.0" = {
        name = "home-or-tmp";
        packageName = "home-or-tmp";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/home-or-tmp/-/home-or-tmp-2.0.0.tgz";
          sha1 = "e36c3f2d2cae7d746a857e38d18d5f32a7882db8";
        };
      };
      "hosted-git-info-2.6.0" = {
        name = "hosted-git-info";
        packageName = "hosted-git-info";
        version = "2.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-2.6.0.tgz";
          sha512 = "1vi4baj130s7mladvb4rad9ka8nqzcwws9i1zaaplp0fdrmnfwjfbpq01cghha2c2frd2f900f07awglhbbyym0bpwmrph0hhhf11ll";
        };
      };
      "hpack.js-2.1.6" = {
        name = "hpack.js";
        packageName = "hpack.js";
        version = "2.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/hpack.js/-/hpack.js-2.1.6.tgz";
          sha1 = "87774c0949e513f42e84575b3c45681fade2a0b2";
        };
      };
      "html-comment-regex-1.1.1" = {
        name = "html-comment-regex";
        packageName = "html-comment-regex";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/html-comment-regex/-/html-comment-regex-1.1.1.tgz";
          sha1 = "668b93776eaae55ebde8f3ad464b307a4963625e";
        };
      };
      "html-encoding-sniffer-1.0.2" = {
        name = "html-encoding-sniffer";
        packageName = "html-encoding-sniffer";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/html-encoding-sniffer/-/html-encoding-sniffer-1.0.2.tgz";
          sha512 = "1ir4rhs26z0x26nr1bca94rbgyginfyrdza1a550ish1ssv5cf9j94dgl2j47n65yvjsnmhx8m749bljr3qjn7pdndcp7l35375jngg";
        };
      };
      "html-entities-1.2.1" = {
        name = "html-entities";
        packageName = "html-entities";
        version = "1.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/html-entities/-/html-entities-1.2.1.tgz";
          sha1 = "0df29351f0721163515dfb9e5543e5f6eed5162f";
        };
      };
      "html-minifier-3.5.10" = {
        name = "html-minifier";
        packageName = "html-minifier";
        version = "3.5.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/html-minifier/-/html-minifier-3.5.10.tgz";
          sha512 = "1kbzriri14n8z4mjn3a8azsvspj4lzdhzp3nsagkja15py5q35k4cf2py19x37bg0irifbi1k4jyn6nn952fqqm2sp0h6484w1j5kz5";
        };
      };
      "html-webpack-plugin-2.30.1" = {
        name = "html-webpack-plugin";
        packageName = "html-webpack-plugin";
        version = "2.30.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/html-webpack-plugin/-/html-webpack-plugin-2.30.1.tgz";
          sha1 = "7f9c421b7ea91ec460f56527d78df484ee7537d5";
        };
      };
      "htmlparser2-3.3.0" = {
        name = "htmlparser2";
        packageName = "htmlparser2";
        version = "3.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.3.0.tgz";
          sha1 = "cc70d05a59f6542e43f0e685c982e14c924a9efe";
        };
      };
      "htmlparser2-3.9.2" = {
        name = "htmlparser2";
        packageName = "htmlparser2";
        version = "3.9.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.9.2.tgz";
          sha1 = "1bdf87acca0f3f9e53fa4fcceb0f4b4cbb00b338";
        };
      };
      "http-deceiver-1.2.7" = {
        name = "http-deceiver";
        packageName = "http-deceiver";
        version = "1.2.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-deceiver/-/http-deceiver-1.2.7.tgz";
          sha1 = "fa7168944ab9a519d337cb0bec7284dc3e723d87";
        };
      };
      "http-errors-1.6.2" = {
        name = "http-errors";
        packageName = "http-errors";
        version = "1.6.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-errors/-/http-errors-1.6.2.tgz";
          sha1 = "0a002cc85707192a7e7946ceedc11155f60ec736";
        };
      };
      "http-parser-js-0.4.11" = {
        name = "http-parser-js";
        packageName = "http-parser-js";
        version = "0.4.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-parser-js/-/http-parser-js-0.4.11.tgz";
          sha512 = "1y4az74zgv7jy1cav126lbrbvfqv30p7v3ijmj91ychbg0337k71cwy2n5dvmdfvwhgv3vxxzw2ymxlbj6wh1wf6aycb313c0xpj920";
        };
      };
      "http-proxy-1.16.2" = {
        name = "http-proxy";
        packageName = "http-proxy";
        version = "1.16.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-proxy/-/http-proxy-1.16.2.tgz";
          sha1 = "06dff292952bf64dbe8471fa9df73066d4f37742";
        };
      };
      "http-proxy-middleware-0.17.4" = {
        name = "http-proxy-middleware";
        packageName = "http-proxy-middleware";
        version = "0.17.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-proxy-middleware/-/http-proxy-middleware-0.17.4.tgz";
          sha1 = "642e8848851d66f09d4f124912846dbaeb41b833";
        };
      };
      "http-signature-1.2.0" = {
        name = "http-signature";
        packageName = "http-signature";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/http-signature/-/http-signature-1.2.0.tgz";
          sha1 = "9aecd925114772f3d95b65a60abb8f7c18fbace1";
        };
      };
      "https-browserify-1.0.0" = {
        name = "https-browserify";
        packageName = "https-browserify";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/https-browserify/-/https-browserify-1.0.0.tgz";
          sha1 = "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73";
        };
      };
      "hullabaloo-config-manager-1.1.1" = {
        name = "hullabaloo-config-manager";
        packageName = "hullabaloo-config-manager";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/hullabaloo-config-manager/-/hullabaloo-config-manager-1.1.1.tgz";
          sha512 = "3w20f5kngfd7wkagpdnj5fbkfbfb930jx3wap0kz4pff9xjs172x3mssqwr636nlz143q3fifg87n0q1dw1r0v0p9knqkkljn8sglnf";
        };
      };
      "iconv-lite-0.4.19" = {
        name = "iconv-lite";
        packageName = "iconv-lite";
        version = "0.4.19";
        src = fetchurl {
          url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.19.tgz";
          sha512 = "0jj1pdq3j9ak8cixn2kjp7ip8hf3xgnb85j4jr32yf9rry620v9072c0kk577mllfk1zl9wzs5ypwzbp7vbhf7j31d5rrqgwb0nldm1";
        };
      };
      "icss-replace-symbols-1.1.0" = {
        name = "icss-replace-symbols";
        packageName = "icss-replace-symbols";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/icss-replace-symbols/-/icss-replace-symbols-1.1.0.tgz";
          sha1 = "06ea6f83679a7749e386cfe1fe812ae5db223ded";
        };
      };
      "icss-utils-2.1.0" = {
        name = "icss-utils";
        packageName = "icss-utils";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/icss-utils/-/icss-utils-2.1.0.tgz";
          sha1 = "83f0a0ec378bf3246178b6c2ad9136f135b1c962";
        };
      };
      "ieee754-1.1.8" = {
        name = "ieee754";
        packageName = "ieee754";
        version = "1.1.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/ieee754/-/ieee754-1.1.8.tgz";
          sha1 = "be33d40ac10ef1926701f6f08a2d86fbfd1ad3e4";
        };
      };
      "iferr-0.1.5" = {
        name = "iferr";
        packageName = "iferr";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/iferr/-/iferr-0.1.5.tgz";
          sha1 = "c60eed69e6d8fdb6b3104a1fcbca1c192dc5b501";
        };
      };
      "ignore-3.3.7" = {
        name = "ignore";
        packageName = "ignore";
        version = "3.3.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/ignore/-/ignore-3.3.7.tgz";
          sha512 = "0f6xhxww989yic6hwdm8mbylcyakfkrrn22a39wdcc9k842xxyyhzfxkmi79s9gjk3rp3h07n265lf4n51z8yafpdm78d617dxbfqb0";
        };
      };
      "ignore-by-default-1.0.1" = {
        name = "ignore-by-default";
        packageName = "ignore-by-default";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ignore-by-default/-/ignore-by-default-1.0.1.tgz";
          sha1 = "48ca6d72f6c6a3af00a9ad4ae6876be3889e2b09";
        };
      };
      "import-lazy-2.1.0" = {
        name = "import-lazy";
        packageName = "import-lazy";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/import-lazy/-/import-lazy-2.1.0.tgz";
          sha1 = "05698e3d45c88e8d7e9d92cb0584e77f096f3e43";
        };
      };
      "import-local-0.1.1" = {
        name = "import-local";
        packageName = "import-local";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/import-local/-/import-local-0.1.1.tgz";
          sha1 = "b1179572aacdc11c6a91009fb430dbcab5f668a8";
        };
      };
      "import-local-1.0.0" = {
        name = "import-local";
        packageName = "import-local";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/import-local/-/import-local-1.0.0.tgz";
          sha512 = "2csjjcjv5rr083zrzz4968g4x6dx02ybr5j9jg03vdav4wzmm9pbfygicji3imd74zrakb8g1glgf7rc08826lzlg3aixla4wg9j1mw";
        };
      };
      "imurmurhash-0.1.4" = {
        name = "imurmurhash";
        packageName = "imurmurhash";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/imurmurhash/-/imurmurhash-0.1.4.tgz";
          sha1 = "9218b9b2b928a238b13dc4fb6b6d576f231453ea";
        };
      };
      "indent-string-2.1.0" = {
        name = "indent-string";
        packageName = "indent-string";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/indent-string/-/indent-string-2.1.0.tgz";
          sha1 = "8e2d48348742121b4a8218b7a137e9a52049dc80";
        };
      };
      "indent-string-3.2.0" = {
        name = "indent-string";
        packageName = "indent-string";
        version = "3.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/indent-string/-/indent-string-3.2.0.tgz";
          sha1 = "4a5fd6d27cc332f37e5419a504dbb837105c9289";
        };
      };
      "indexes-of-1.0.1" = {
        name = "indexes-of";
        packageName = "indexes-of";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/indexes-of/-/indexes-of-1.0.1.tgz";
          sha1 = "f30f716c8e2bd346c7b67d3df3915566a7c05607";
        };
      };
      "indexof-0.0.1" = {
        name = "indexof";
        packageName = "indexof";
        version = "0.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/indexof/-/indexof-0.0.1.tgz";
          sha1 = "82dc336d232b9062179d05ab3293a66059fd435d";
        };
      };
      "inflight-1.0.6" = {
        name = "inflight";
        packageName = "inflight";
        version = "1.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
          sha1 = "49bd6331d7d02d0c09bc910a1075ba8165b56df9";
        };
      };
      "inherits-2.0.1" = {
        name = "inherits";
        packageName = "inherits";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/inherits/-/inherits-2.0.1.tgz";
          sha1 = "b17d08d326b4423e568eff719f91b0b1cbdf69f1";
        };
      };
      "inherits-2.0.3" = {
        name = "inherits";
        packageName = "inherits";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz";
          sha1 = "633c2c83e3da42a502f52466022480f4208261de";
        };
      };
      "ini-1.3.5" = {
        name = "ini";
        packageName = "ini";
        version = "1.3.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/ini/-/ini-1.3.5.tgz";
          sha512 = "1rjbvf1rg5ywhnba08sgagn2qf23lab330qrqmh7d891zap3xpxcyfyj1cblpf0f0rypglcfacybzyrpd4996aa1mbc820awa33k5j5";
        };
      };
      "internal-ip-1.2.0" = {
        name = "internal-ip";
        packageName = "internal-ip";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/internal-ip/-/internal-ip-1.2.0.tgz";
          sha1 = "ae9fbf93b984878785d50a8de1b356956058cf5c";
        };
      };
      "interpret-1.1.0" = {
        name = "interpret";
        packageName = "interpret";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/interpret/-/interpret-1.1.0.tgz";
          sha1 = "7ed1b1410c6a0e0f78cf95d3b8440c63f78b8614";
        };
      };
      "invariant-2.2.3" = {
        name = "invariant";
        packageName = "invariant";
        version = "2.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/invariant/-/invariant-2.2.3.tgz";
          sha512 = "2s1gvqa56gnq2pkh0yy5ch3pb69dkwxr6myw9r3cc1c55xnrpiwzgc90ns4n9lij9slw0aayzm95k3x3rfrv0m7dnikq39hx0ylz7pd";
        };
      };
      "invert-kv-1.0.0" = {
        name = "invert-kv";
        packageName = "invert-kv";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/invert-kv/-/invert-kv-1.0.0.tgz";
          sha1 = "104a8e4aaca6d3d8cd157a8ef8bfab2d7a3ffdb6";
        };
      };
      "ip-1.1.5" = {
        name = "ip";
        packageName = "ip";
        version = "1.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/ip/-/ip-1.1.5.tgz";
          sha1 = "bdded70114290828c0a039e72ef25f5aaec4354a";
        };
      };
      "ipaddr.js-1.6.0" = {
        name = "ipaddr.js";
        packageName = "ipaddr.js";
        version = "1.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-1.6.0.tgz";
          sha1 = "e3fa357b773da619f26e95f049d055c72796f86b";
        };
      };
      "irregular-plurals-1.4.0" = {
        name = "irregular-plurals";
        packageName = "irregular-plurals";
        version = "1.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/irregular-plurals/-/irregular-plurals-1.4.0.tgz";
          sha1 = "2ca9b033651111855412f16be5d77c62a458a766";
        };
      };
      "is-absolute-url-2.1.0" = {
        name = "is-absolute-url";
        packageName = "is-absolute-url";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-absolute-url/-/is-absolute-url-2.1.0.tgz";
          sha1 = "50530dfb84fcc9aa7dbe7852e83a37b93b9f2aa6";
        };
      };
      "is-accessor-descriptor-0.1.6" = {
        name = "is-accessor-descriptor";
        packageName = "is-accessor-descriptor";
        version = "0.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-0.1.6.tgz";
          sha1 = "a9e12cb3ae8d876727eeef3843f8a0897b5c98d6";
        };
      };
      "is-accessor-descriptor-1.0.0" = {
        name = "is-accessor-descriptor";
        packageName = "is-accessor-descriptor";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-1.0.0.tgz";
          sha512 = "1qllik6fjwfq17ic0fxwqyll8mrhmcm36xfsq45xc57mq9ah4i4nn4f8fvgb0gx4kpl3jlpkzndp0xlmmf2mh0xmggw6mhw74fng64v";
        };
      };
      "is-arrayish-0.2.1" = {
        name = "is-arrayish";
        packageName = "is-arrayish";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz";
          sha1 = "77c99840527aa8ecb1a8ba697b80645a7a926a9d";
        };
      };
      "is-arrayish-0.3.1" = {
        name = "is-arrayish";
        packageName = "is-arrayish";
        version = "0.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.3.1.tgz";
          sha1 = "c2dfc386abaa0c3e33c48db3fe87059e69065efd";
        };
      };
      "is-binary-path-1.0.1" = {
        name = "is-binary-path";
        packageName = "is-binary-path";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-binary-path/-/is-binary-path-1.0.1.tgz";
          sha1 = "75f16642b480f187a711c814161fd3a4a7655898";
        };
      };
      "is-buffer-1.1.6" = {
        name = "is-buffer";
        packageName = "is-buffer";
        version = "1.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.6.tgz";
          sha512 = "3kr8dm9qyklmm2xyiz75s8db90bfilfals4x0g276kncihrrrz0ar4y6dqpvc7pwy7h43jay1bayi1r62x97nzvcswkk4ap18pl1irm";
        };
      };
      "is-builtin-module-1.0.0" = {
        name = "is-builtin-module";
        packageName = "is-builtin-module";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-builtin-module/-/is-builtin-module-1.0.0.tgz";
          sha1 = "540572d34f7ac3119f8f76c30cbc1b1e037affbe";
        };
      };
      "is-callable-1.1.3" = {
        name = "is-callable";
        packageName = "is-callable";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-callable/-/is-callable-1.1.3.tgz";
          sha1 = "86eb75392805ddc33af71c92a0eedf74ee7604b2";
        };
      };
      "is-ci-1.1.0" = {
        name = "is-ci";
        packageName = "is-ci";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-ci/-/is-ci-1.1.0.tgz";
          sha512 = "0m66alrh568wj40xwshf8q99gsjfk1jr0czp4jc2sm519wfzzzprkl5zjvw2r5h49p72d50ywj9qg67dnyazq0ijy4flgny2b1ygd3k";
        };
      };
      "is-data-descriptor-0.1.4" = {
        name = "is-data-descriptor";
        packageName = "is-data-descriptor";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-0.1.4.tgz";
          sha1 = "0b5ee648388e2c860282e793f1856fec3f301b56";
        };
      };
      "is-data-descriptor-1.0.0" = {
        name = "is-data-descriptor";
        packageName = "is-data-descriptor";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-1.0.0.tgz";
          sha512 = "0ny6kxc752fg3z6fmj8a7fw2lai2y17d9fx0028nvyv1qj0sa30rfryhv9xd7b7is1yfs0val6amsy2b22rh589il10md36a75mgd4d";
        };
      };
      "is-date-object-1.0.1" = {
        name = "is-date-object";
        packageName = "is-date-object";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-date-object/-/is-date-object-1.0.1.tgz";
          sha1 = "9aa20eb6aeebbff77fbd33e74ca01b33581d3a16";
        };
      };
      "is-descriptor-0.1.6" = {
        name = "is-descriptor";
        packageName = "is-descriptor";
        version = "0.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-descriptor/-/is-descriptor-0.1.6.tgz";
          sha512 = "0gbflcxmd30gzj91y19fylsfalirl6qg71sxjximc8lc2vxkg5h9scnahvxsczymchlx742i8ai489843ys431vyw73rp418jpxiw3a";
        };
      };
      "is-descriptor-1.0.2" = {
        name = "is-descriptor";
        packageName = "is-descriptor";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-descriptor/-/is-descriptor-1.0.2.tgz";
          sha512 = "2v1a9mn2rzz52v8vs3i7njk9pv95fh971yc81xr0zkaw3dff4gbv1zv048xyjysfgwpajbyryk2px8hinwwh0wagblmw6chdbjsrs6r";
        };
      };
      "is-directory-0.3.1" = {
        name = "is-directory";
        packageName = "is-directory";
        version = "0.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-directory/-/is-directory-0.3.1.tgz";
          sha1 = "61339b6f2475fc772fd9c9d83f5c8575dc154ae1";
        };
      };
      "is-dotfile-1.0.3" = {
        name = "is-dotfile";
        packageName = "is-dotfile";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-dotfile/-/is-dotfile-1.0.3.tgz";
          sha1 = "a6a2f32ffd2dfb04f5ca25ecd0f6b83cf798a1e1";
        };
      };
      "is-equal-shallow-0.1.3" = {
        name = "is-equal-shallow";
        packageName = "is-equal-shallow";
        version = "0.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-equal-shallow/-/is-equal-shallow-0.1.3.tgz";
          sha1 = "2238098fc221de0bcfa5d9eac4c45d638aa1c534";
        };
      };
      "is-error-2.2.1" = {
        name = "is-error";
        packageName = "is-error";
        version = "2.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-error/-/is-error-2.2.1.tgz";
          sha1 = "684a96d84076577c98f4cdb40c6d26a5123bf19c";
        };
      };
      "is-extendable-0.1.1" = {
        name = "is-extendable";
        packageName = "is-extendable";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz";
          sha1 = "62b110e289a471418e3ec36a617d472e301dfc89";
        };
      };
      "is-extendable-1.0.1" = {
        name = "is-extendable";
        packageName = "is-extendable";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-extendable/-/is-extendable-1.0.1.tgz";
          sha512 = "0w73qlx9ynmv2iznw1kll86yd04z4rsz3788nzgh7amcnpsbyxbrs734im9dibqgps6pjyz61s8kp4lcsbjsdfrlc51m1pm2hrxgfba";
        };
      };
      "is-extglob-1.0.0" = {
        name = "is-extglob";
        packageName = "is-extglob";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-extglob/-/is-extglob-1.0.0.tgz";
          sha1 = "ac468177c4943405a092fc8f29760c6ffc6206c0";
        };
      };
      "is-extglob-2.1.1" = {
        name = "is-extglob";
        packageName = "is-extglob";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz";
          sha1 = "a88c02535791f02ed37c76a1b9ea9773c833f8c2";
        };
      };
      "is-finite-1.0.2" = {
        name = "is-finite";
        packageName = "is-finite";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-finite/-/is-finite-1.0.2.tgz";
          sha1 = "cc6677695602be550ef11e8b4aa6305342b6d0aa";
        };
      };
      "is-fullwidth-code-point-1.0.0" = {
        name = "is-fullwidth-code-point";
        packageName = "is-fullwidth-code-point";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-1.0.0.tgz";
          sha1 = "ef9e31386f031a7f0d643af82fde50c457ef00cb";
        };
      };
      "is-fullwidth-code-point-2.0.0" = {
        name = "is-fullwidth-code-point";
        packageName = "is-fullwidth-code-point";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-2.0.0.tgz";
          sha1 = "a3b30a5c4f199183167aaab93beefae3ddfb654f";
        };
      };
      "is-generator-fn-1.0.0" = {
        name = "is-generator-fn";
        packageName = "is-generator-fn";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-generator-fn/-/is-generator-fn-1.0.0.tgz";
          sha1 = "969d49e1bb3329f6bb7f09089be26578b2ddd46a";
        };
      };
      "is-glob-2.0.1" = {
        name = "is-glob";
        packageName = "is-glob";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-glob/-/is-glob-2.0.1.tgz";
          sha1 = "d096f926a3ded5600f3fdfd91198cb0888c2d863";
        };
      };
      "is-glob-3.1.0" = {
        name = "is-glob";
        packageName = "is-glob";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-glob/-/is-glob-3.1.0.tgz";
          sha1 = "7ba5ae24217804ac70707b96922567486cc3e84a";
        };
      };
      "is-glob-4.0.0" = {
        name = "is-glob";
        packageName = "is-glob";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-glob/-/is-glob-4.0.0.tgz";
          sha1 = "9521c76845cc2610a85203ddf080a958c2ffabc0";
        };
      };
      "is-installed-globally-0.1.0" = {
        name = "is-installed-globally";
        packageName = "is-installed-globally";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-installed-globally/-/is-installed-globally-0.1.0.tgz";
          sha1 = "0dfd98f5a9111716dd535dda6492f67bf3d25a80";
        };
      };
      "is-npm-1.0.0" = {
        name = "is-npm";
        packageName = "is-npm";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-npm/-/is-npm-1.0.0.tgz";
          sha1 = "f2fb63a65e4905b406c86072765a1a4dc793b9f4";
        };
      };
      "is-number-2.1.0" = {
        name = "is-number";
        packageName = "is-number";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-number/-/is-number-2.1.0.tgz";
          sha1 = "01fcbbb393463a548f2f466cce16dece49db908f";
        };
      };
      "is-number-3.0.0" = {
        name = "is-number";
        packageName = "is-number";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-number/-/is-number-3.0.0.tgz";
          sha1 = "24fd6201a4782cf50561c810276afc7d12d71195";
        };
      };
      "is-number-4.0.0" = {
        name = "is-number";
        packageName = "is-number";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-number/-/is-number-4.0.0.tgz";
          sha512 = "20ybhnkgsrcwv730ch8qvzayjwcvs83jk2w6iim7wv3h2nn4jz8hsldn0k6ggx4xgcypwirqjb5xdacsswsk06w2nk56zr509q2aadd";
        };
      };
      "is-obj-1.0.1" = {
        name = "is-obj";
        packageName = "is-obj";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-obj/-/is-obj-1.0.1.tgz";
          sha1 = "3e4729ac1f5fde025cd7d83a896dab9f4f67db0f";
        };
      };
      "is-observable-0.2.0" = {
        name = "is-observable";
        packageName = "is-observable";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-observable/-/is-observable-0.2.0.tgz";
          sha1 = "b361311d83c6e5d726cabf5e250b0237106f5ae2";
        };
      };
      "is-odd-2.0.0" = {
        name = "is-odd";
        packageName = "is-odd";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-odd/-/is-odd-2.0.0.tgz";
          sha512 = "34m1wg28c9l1v9bqz2klwl4ybhyqkhk80d95664jmcbq1jjpg471nv96mqgqy4838xpa8wm7mbpynmq4294pq6iw163s0ar1b3a4f1r";
        };
      };
      "is-path-cwd-1.0.0" = {
        name = "is-path-cwd";
        packageName = "is-path-cwd";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-path-cwd/-/is-path-cwd-1.0.0.tgz";
          sha1 = "d225ec23132e89edd38fda767472e62e65f1106d";
        };
      };
      "is-path-in-cwd-1.0.0" = {
        name = "is-path-in-cwd";
        packageName = "is-path-in-cwd";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-path-in-cwd/-/is-path-in-cwd-1.0.0.tgz";
          sha1 = "6477582b8214d602346094567003be8a9eac04dc";
        };
      };
      "is-path-inside-1.0.1" = {
        name = "is-path-inside";
        packageName = "is-path-inside";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-path-inside/-/is-path-inside-1.0.1.tgz";
          sha1 = "8ef5b7de50437a3fdca6b4e865ef7aa55cb48036";
        };
      };
      "is-plain-obj-1.1.0" = {
        name = "is-plain-obj";
        packageName = "is-plain-obj";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-plain-obj/-/is-plain-obj-1.1.0.tgz";
          sha1 = "71a50c8429dfca773c92a390a4a03b39fcd51d3e";
        };
      };
      "is-plain-object-2.0.4" = {
        name = "is-plain-object";
        packageName = "is-plain-object";
        version = "2.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-plain-object/-/is-plain-object-2.0.4.tgz";
          sha512 = "0xgsjz9m3kg5pm36lcchblxk53qay59ya7wi5jgdmz0dsl5b0j2j7wcd48yyfaip1m70mj9aqf8kib02fn62k0hy0vxg2hng60yk4w7";
        };
      };
      "is-posix-bracket-0.1.1" = {
        name = "is-posix-bracket";
        packageName = "is-posix-bracket";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-posix-bracket/-/is-posix-bracket-0.1.1.tgz";
          sha1 = "3334dc79774368e92f016e6fbc0a88f5cd6e6bc4";
        };
      };
      "is-primitive-2.0.0" = {
        name = "is-primitive";
        packageName = "is-primitive";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-primitive/-/is-primitive-2.0.0.tgz";
          sha1 = "207bab91638499c07b2adf240a41a87210034575";
        };
      };
      "is-promise-2.1.0" = {
        name = "is-promise";
        packageName = "is-promise";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-promise/-/is-promise-2.1.0.tgz";
          sha1 = "79a2a9ece7f096e80f36d2b2f3bc16c1ff4bf3fa";
        };
      };
      "is-redirect-1.0.0" = {
        name = "is-redirect";
        packageName = "is-redirect";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-redirect/-/is-redirect-1.0.0.tgz";
          sha1 = "1d03dded53bd8db0f30c26e4f95d36fc7c87dc24";
        };
      };
      "is-regex-1.0.4" = {
        name = "is-regex";
        packageName = "is-regex";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-regex/-/is-regex-1.0.4.tgz";
          sha1 = "5517489b547091b0930e095654ced25ee97e9491";
        };
      };
      "is-retry-allowed-1.1.0" = {
        name = "is-retry-allowed";
        packageName = "is-retry-allowed";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-retry-allowed/-/is-retry-allowed-1.1.0.tgz";
          sha1 = "11a060568b67339444033d0125a61a20d564fb34";
        };
      };
      "is-stream-1.1.0" = {
        name = "is-stream";
        packageName = "is-stream";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-stream/-/is-stream-1.1.0.tgz";
          sha1 = "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44";
        };
      };
      "is-svg-2.1.0" = {
        name = "is-svg";
        packageName = "is-svg";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-svg/-/is-svg-2.1.0.tgz";
          sha1 = "cf61090da0d9efbcab8722deba6f032208dbb0e9";
        };
      };
      "is-symbol-1.0.1" = {
        name = "is-symbol";
        packageName = "is-symbol";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-symbol/-/is-symbol-1.0.1.tgz";
          sha1 = "3cc59f00025194b6ab2e38dbae6689256b660572";
        };
      };
      "is-typedarray-1.0.0" = {
        name = "is-typedarray";
        packageName = "is-typedarray";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz";
          sha1 = "e479c80858df0c1b11ddda6940f96011fcda4a9a";
        };
      };
      "is-url-1.2.2" = {
        name = "is-url";
        packageName = "is-url";
        version = "1.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-url/-/is-url-1.2.2.tgz";
          sha1 = "498905a593bf47cc2d9e7f738372bbf7696c7f26";
        };
      };
      "is-utf8-0.2.1" = {
        name = "is-utf8";
        packageName = "is-utf8";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-utf8/-/is-utf8-0.2.1.tgz";
          sha1 = "4b0da1442104d1b336340e80797e865cf39f7d72";
        };
      };
      "is-windows-1.0.2" = {
        name = "is-windows";
        packageName = "is-windows";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-windows/-/is-windows-1.0.2.tgz";
          sha512 = "0h9a8zw2ddl25yryp7i376iqjdvlsx4gw93sxy4067dw2yi98m3krfwd9xgi9q5w0idw9rqnyhhncr38xsppyi5izkb7ngai58bawkr";
        };
      };
      "is-wsl-1.1.0" = {
        name = "is-wsl";
        packageName = "is-wsl";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/is-wsl/-/is-wsl-1.1.0.tgz";
          sha1 = "1f16e4aa22b04d1336b66188a66af3c600c3a66d";
        };
      };
      "isarray-0.0.1" = {
        name = "isarray";
        packageName = "isarray";
        version = "0.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/isarray/-/isarray-0.0.1.tgz";
          sha1 = "8a18acfca9a8f4177e09abfc6038939b05d1eedf";
        };
      };
      "isarray-1.0.0" = {
        name = "isarray";
        packageName = "isarray";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz";
          sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
        };
      };
      "isarray-2.0.1" = {
        name = "isarray";
        packageName = "isarray";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/isarray/-/isarray-2.0.1.tgz";
          sha1 = "a37d94ed9cda2d59865c9f76fe596ee1f338741e";
        };
      };
      "isexe-2.0.0" = {
        name = "isexe";
        packageName = "isexe";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz";
          sha1 = "e8fbf374dc556ff8947a10dcb0572d633f2cfa10";
        };
      };
      "isnumeric-0.2.0" = {
        name = "isnumeric";
        packageName = "isnumeric";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/isnumeric/-/isnumeric-0.2.0.tgz";
          sha1 = "a2347ba360de19e33d0ffd590fddf7755cbf2e64";
        };
      };
      "isobject-2.1.0" = {
        name = "isobject";
        packageName = "isobject";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/isobject/-/isobject-2.1.0.tgz";
          sha1 = "f065561096a3f1da2ef46272f815c840d87e0c89";
        };
      };
      "isobject-3.0.1" = {
        name = "isobject";
        packageName = "isobject";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/isobject/-/isobject-3.0.1.tgz";
          sha1 = "4e431e92b11a9731636aa1f9c8d1ccbcfdab78df";
        };
      };
      "isomorphic-fetch-2.2.1" = {
        name = "isomorphic-fetch";
        packageName = "isomorphic-fetch";
        version = "2.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/isomorphic-fetch/-/isomorphic-fetch-2.2.1.tgz";
          sha1 = "611ae1acf14f5e81f729507472819fe9733558a9";
        };
      };
      "isstream-0.1.2" = {
        name = "isstream";
        packageName = "isstream";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz";
          sha1 = "47e63f7af55afa6f92e1500e690eb8b8529c099a";
        };
      };
      "js-base64-2.4.3" = {
        name = "js-base64";
        packageName = "js-base64";
        version = "2.4.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-base64/-/js-base64-2.4.3.tgz";
          sha512 = "09rdlbny3prrfs56vf83jx9vv8w23aga68wp4lz4an711lkji1byy70dgxhgyn19p81spcq84nhjgf4zaq1jdys6b1jpq1pndh2pc8z";
        };
      };
      "js-polyfills-0.1.42" = {
        name = "js-polyfills";
        packageName = "js-polyfills";
        version = "0.1.42";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-polyfills/-/js-polyfills-0.1.42.tgz";
          sha1 = "5d484902b361e3cf601fd23ad0f30bafcc93f148";
        };
      };
      "js-string-escape-1.0.1" = {
        name = "js-string-escape";
        packageName = "js-string-escape";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-string-escape/-/js-string-escape-1.0.1.tgz";
          sha1 = "e2625badbc0d67c7533e9edc1068c587ae4137ef";
        };
      };
      "js-tokens-3.0.2" = {
        name = "js-tokens";
        packageName = "js-tokens";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-tokens/-/js-tokens-3.0.2.tgz";
          sha1 = "9866df395102130e38f7f996bceb65443209c25b";
        };
      };
      "js-yaml-3.11.0" = {
        name = "js-yaml";
        packageName = "js-yaml";
        version = "3.11.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.11.0.tgz";
          sha512 = "0gka65n4d9gmcy0c8cy5h55r273dbxnw54gibp2nq5mmdmksjgb2nhcdfgfxs1wg3yayyrydn2v79fny7hdyq907dg87vmgjnsnr8mi";
        };
      };
      "js-yaml-3.7.0" = {
        name = "js-yaml";
        packageName = "js-yaml";
        version = "3.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.7.0.tgz";
          sha1 = "5c967ddd837a9bfdca5f2de84253abe8a1c03b80";
        };
      };
      "jsbn-0.1.1" = {
        name = "jsbn";
        packageName = "jsbn";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/jsbn/-/jsbn-0.1.1.tgz";
          sha1 = "a5e654c2e5a2deb5f201d96cefbca80c0ef2f513";
        };
      };
      "jsdom-11.6.2" = {
        name = "jsdom";
        packageName = "jsdom";
        version = "11.6.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/jsdom/-/jsdom-11.6.2.tgz";
          sha512 = "3fx2ic79425aplpy8dwh3clhzsldrc2jgzxhkcyslwhyi7d7fm9yxydn5a1bnqicdd63dgqdg71n297a30fj4h7r5wlm5fjjs39j1x4";
        };
      };
      "jsesc-0.5.0" = {
        name = "jsesc";
        packageName = "jsesc";
        version = "0.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz";
          sha1 = "e7dee66e35d6fc16f710fe91d5cf69f70f08911d";
        };
      };
      "jsesc-1.3.0" = {
        name = "jsesc";
        packageName = "jsesc";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/jsesc/-/jsesc-1.3.0.tgz";
          sha1 = "46c3fec8c1892b12b0833db9bc7622176dbab34b";
        };
      };
      "json-loader-0.5.7" = {
        name = "json-loader";
        packageName = "json-loader";
        version = "0.5.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/json-loader/-/json-loader-0.5.7.tgz";
          sha512 = "3iwy9jwca9hg6h1k7cmcdlsygn2qzjv7w72fsrfjfpdrcyd4xc5fb11sf664rvnzrfmz24f19kvi3qawif4n63lggvpg5pv73qfrcs0";
        };
      };
      "json-parse-better-errors-1.0.1" = {
        name = "json-parse-better-errors";
        packageName = "json-parse-better-errors";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/json-parse-better-errors/-/json-parse-better-errors-1.0.1.tgz";
          sha512 = "05ndp7b03ikx2vqivfxlm6c73yagjyrdp22ay8z592pqxldbsm7hjzpa3asal2vys99lvirqar3ly3sb1ibhhngls4sqc4nwp2jj967";
        };
      };
      "json-schema-0.2.3" = {
        name = "json-schema";
        packageName = "json-schema";
        version = "0.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/json-schema/-/json-schema-0.2.3.tgz";
          sha1 = "b480c892e59a2f05954ce727bd3f2a4e882f9e13";
        };
      };
      "json-schema-traverse-0.3.1" = {
        name = "json-schema-traverse";
        packageName = "json-schema-traverse";
        version = "0.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.3.1.tgz";
          sha1 = "349a6d44c53a51de89b40805c5d5e59b417d3340";
        };
      };
      "json-stringify-safe-5.0.1" = {
        name = "json-stringify-safe";
        packageName = "json-stringify-safe";
        version = "5.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz";
          sha1 = "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb";
        };
      };
      "json3-3.3.2" = {
        name = "json3";
        packageName = "json3";
        version = "3.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/json3/-/json3-3.3.2.tgz";
          sha1 = "3c0434743df93e2f5c42aee7b19bcb483575f4e1";
        };
      };
      "json5-0.5.1" = {
        name = "json5";
        packageName = "json5";
        version = "0.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/json5/-/json5-0.5.1.tgz";
          sha1 = "1eade7acc012034ad84e2396767ead9fa5495821";
        };
      };
      "jsprim-1.4.1" = {
        name = "jsprim";
        packageName = "jsprim";
        version = "1.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/jsprim/-/jsprim-1.4.1.tgz";
          sha1 = "313e66bc1e5cc06e438bc1b7499c2e5c56acb6a2";
        };
      };
      "killable-1.0.0" = {
        name = "killable";
        packageName = "killable";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/killable/-/killable-1.0.0.tgz";
          sha1 = "da8b84bd47de5395878f95d64d02f2449fe05e6b";
        };
      };
      "kind-of-3.2.2" = {
        name = "kind-of";
        packageName = "kind-of";
        version = "3.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/kind-of/-/kind-of-3.2.2.tgz";
          sha1 = "31ea21a734bab9bbb0f32466d893aea51e4a3c64";
        };
      };
      "kind-of-4.0.0" = {
        name = "kind-of";
        packageName = "kind-of";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/kind-of/-/kind-of-4.0.0.tgz";
          sha1 = "20813df3d712928b207378691a45066fae72dd57";
        };
      };
      "kind-of-5.1.0" = {
        name = "kind-of";
        packageName = "kind-of";
        version = "5.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/kind-of/-/kind-of-5.1.0.tgz";
          sha512 = "0zk87sccrjx6pgf9n74v4msnqwq5siyhrkpaklx7yk85ygy5ypcgmyfhbd5mmcyd53x8zcw0gzvp9bhbglziqbhp7a6n5zsf6p08q9l";
        };
      };
      "kind-of-6.0.2" = {
        name = "kind-of";
        packageName = "kind-of";
        version = "6.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/kind-of/-/kind-of-6.0.2.tgz";
          sha512 = "2l91vcracq8y3nxacsssb4yhk0ww011gi5sn55wsb6bpnhyds2i1x98512f61r8awxmj602bxky6c7hsyibjvz17f1pmlf7r4whp6dk";
        };
      };
      "last-line-stream-1.0.0" = {
        name = "last-line-stream";
        packageName = "last-line-stream";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/last-line-stream/-/last-line-stream-1.0.0.tgz";
          sha1 = "d1b64d69f86ff24af2d04883a2ceee14520a5600";
        };
      };
      "latest-version-3.1.0" = {
        name = "latest-version";
        packageName = "latest-version";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/latest-version/-/latest-version-3.1.0.tgz";
          sha1 = "a205383fea322b33b5ae3b18abee0dc2f356ee15";
        };
      };
      "lazy-cache-1.0.4" = {
        name = "lazy-cache";
        packageName = "lazy-cache";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/lazy-cache/-/lazy-cache-1.0.4.tgz";
          sha1 = "a1d78fc3a50474cb80845d3b3b6e1da49a446e8e";
        };
      };
      "lazy-cache-2.0.2" = {
        name = "lazy-cache";
        packageName = "lazy-cache";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/lazy-cache/-/lazy-cache-2.0.2.tgz";
          sha1 = "b9190a4f913354694840859f8a8f7084d8822264";
        };
      };
      "lcid-1.0.0" = {
        name = "lcid";
        packageName = "lcid";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lcid/-/lcid-1.0.0.tgz";
          sha1 = "308accafa0bc483a3867b4b6f2b9506251d1b835";
        };
      };
      "left-pad-1.2.0" = {
        name = "left-pad";
        packageName = "left-pad";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/left-pad/-/left-pad-1.2.0.tgz";
          sha1 = "d30a73c6b8201d8f7d8e7956ba9616087a68e0ee";
        };
      };
      "levn-0.3.0" = {
        name = "levn";
        packageName = "levn";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
          sha1 = "3b09924edf9f083c0490fdd4c0bc4421e04764ee";
        };
      };
      "load-json-file-1.1.0" = {
        name = "load-json-file";
        packageName = "load-json-file";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/load-json-file/-/load-json-file-1.1.0.tgz";
          sha1 = "956905708d58b4bab4c2261b04f59f31c99374c0";
        };
      };
      "load-json-file-2.0.0" = {
        name = "load-json-file";
        packageName = "load-json-file";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/load-json-file/-/load-json-file-2.0.0.tgz";
          sha1 = "7947e42149af80d696cbf797bcaabcfe1fe29ca8";
        };
      };
      "load-json-file-4.0.0" = {
        name = "load-json-file";
        packageName = "load-json-file";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/load-json-file/-/load-json-file-4.0.0.tgz";
          sha1 = "2f5f45ab91e33216234fd53adab668eb4ec0993b";
        };
      };
      "loader-runner-2.3.0" = {
        name = "loader-runner";
        packageName = "loader-runner";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/loader-runner/-/loader-runner-2.3.0.tgz";
          sha1 = "f482aea82d543e07921700d5a46ef26fdac6b8a2";
        };
      };
      "loader-utils-0.2.17" = {
        name = "loader-utils";
        packageName = "loader-utils";
        version = "0.2.17";
        src = fetchurl {
          url = "https://registry.npmjs.org/loader-utils/-/loader-utils-0.2.17.tgz";
          sha1 = "f86e6374d43205a6e6c60e9196f17c0299bfb348";
        };
      };
      "loader-utils-1.1.0" = {
        name = "loader-utils";
        packageName = "loader-utils";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/loader-utils/-/loader-utils-1.1.0.tgz";
          sha1 = "c98aef488bcceda2ffb5e2de646d6a754429f5cd";
        };
      };
      "locate-path-2.0.0" = {
        name = "locate-path";
        packageName = "locate-path";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/locate-path/-/locate-path-2.0.0.tgz";
          sha1 = "2b568b265eec944c6d9c0de9c3dbbbca0354cd8e";
        };
      };
      "lodash-4.17.5" = {
        name = "lodash";
        packageName = "lodash";
        version = "4.17.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash/-/lodash-4.17.5.tgz";
          sha512 = "11hikgyas884mz8a58vyixaahxbpdwljdw4cb6qp15xa3sfqikp2mm6wgv41jsl34nzsv1hkx9kw3nwczvas5p73whirmaz4sxggwmj";
        };
      };
      "lodash._reinterpolate-3.0.0" = {
        name = "lodash._reinterpolate";
        packageName = "lodash._reinterpolate";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash._reinterpolate/-/lodash._reinterpolate-3.0.0.tgz";
          sha1 = "0ccf2d89166af03b3663c796538b75ac6e114d9d";
        };
      };
      "lodash.camelcase-4.3.0" = {
        name = "lodash.camelcase";
        packageName = "lodash.camelcase";
        version = "4.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.camelcase/-/lodash.camelcase-4.3.0.tgz";
          sha1 = "b28aa6288a2b9fc651035c7711f65ab6190331a6";
        };
      };
      "lodash.clonedeep-4.5.0" = {
        name = "lodash.clonedeep";
        packageName = "lodash.clonedeep";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.clonedeep/-/lodash.clonedeep-4.5.0.tgz";
          sha1 = "e23f3f9c4f8fbdde872529c1071857a086e5ccef";
        };
      };
      "lodash.clonedeepwith-4.5.0" = {
        name = "lodash.clonedeepwith";
        packageName = "lodash.clonedeepwith";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.clonedeepwith/-/lodash.clonedeepwith-4.5.0.tgz";
          sha1 = "6ee30573a03a1a60d670a62ef33c10cf1afdbdd4";
        };
      };
      "lodash.debounce-4.0.8" = {
        name = "lodash.debounce";
        packageName = "lodash.debounce";
        version = "4.0.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.debounce/-/lodash.debounce-4.0.8.tgz";
          sha1 = "82d79bff30a67c4005ffd5e2515300ad9ca4d7af";
        };
      };
      "lodash.difference-4.5.0" = {
        name = "lodash.difference";
        packageName = "lodash.difference";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.difference/-/lodash.difference-4.5.0.tgz";
          sha1 = "9ccb4e505d486b91651345772885a2df27fd017c";
        };
      };
      "lodash.flatten-4.4.0" = {
        name = "lodash.flatten";
        packageName = "lodash.flatten";
        version = "4.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.flatten/-/lodash.flatten-4.4.0.tgz";
          sha1 = "f31c22225a9632d2bbf8e4addbef240aa765a61f";
        };
      };
      "lodash.flattendeep-4.4.0" = {
        name = "lodash.flattendeep";
        packageName = "lodash.flattendeep";
        version = "4.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.flattendeep/-/lodash.flattendeep-4.4.0.tgz";
          sha1 = "fb030917f86a3134e5bc9bec0d69e0013ddfedb2";
        };
      };
      "lodash.isequal-4.5.0" = {
        name = "lodash.isequal";
        packageName = "lodash.isequal";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.isequal/-/lodash.isequal-4.5.0.tgz";
          sha1 = "415c4478f2bcc30120c22ce10ed3226f7d3e18e0";
        };
      };
      "lodash.memoize-4.1.2" = {
        name = "lodash.memoize";
        packageName = "lodash.memoize";
        version = "4.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.memoize/-/lodash.memoize-4.1.2.tgz";
          sha1 = "bcc6c49a42a2840ed997f323eada5ecd182e0bfe";
        };
      };
      "lodash.merge-4.6.1" = {
        name = "lodash.merge";
        packageName = "lodash.merge";
        version = "4.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.merge/-/lodash.merge-4.6.1.tgz";
          sha512 = "0lv358i7xyg1hn90fk3xzih5kmj1zlmr759g0hh9fa7npbik629q8h5jd2lvjnrm76ciz92k0yn5z6vcqdbgws1zzsrqzw7ixmk7rh0";
        };
      };
      "lodash.sortby-4.7.0" = {
        name = "lodash.sortby";
        packageName = "lodash.sortby";
        version = "4.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.sortby/-/lodash.sortby-4.7.0.tgz";
          sha1 = "edd14c824e2cc9c1e0b0a1b42bb5210516a42438";
        };
      };
      "lodash.template-4.4.0" = {
        name = "lodash.template";
        packageName = "lodash.template";
        version = "4.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.template/-/lodash.template-4.4.0.tgz";
          sha1 = "e73a0385c8355591746e020b99679c690e68fba0";
        };
      };
      "lodash.templatesettings-4.1.0" = {
        name = "lodash.templatesettings";
        packageName = "lodash.templatesettings";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.templatesettings/-/lodash.templatesettings-4.1.0.tgz";
          sha1 = "2b4d4e95ba440d915ff08bc899e4553666713316";
        };
      };
      "lodash.uniq-4.5.0" = {
        name = "lodash.uniq";
        packageName = "lodash.uniq";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lodash.uniq/-/lodash.uniq-4.5.0.tgz";
          sha1 = "d0225373aeb652adc1bc82e4945339a842754773";
        };
      };
      "loglevel-1.6.1" = {
        name = "loglevel";
        packageName = "loglevel";
        version = "1.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/loglevel/-/loglevel-1.6.1.tgz";
          sha1 = "e0fc95133b6ef276cdc8887cdaf24aa6f156f8fa";
        };
      };
      "longest-1.0.1" = {
        name = "longest";
        packageName = "longest";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/longest/-/longest-1.0.1.tgz";
          sha1 = "30a0b2da38f73770e8294a0d22e6625ed77d0097";
        };
      };
      "loose-envify-1.3.1" = {
        name = "loose-envify";
        packageName = "loose-envify";
        version = "1.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/loose-envify/-/loose-envify-1.3.1.tgz";
          sha1 = "d1a8ad33fa9ce0e713d65fdd0ac8b748d478c848";
        };
      };
      "lost-8.2.1" = {
        name = "lost";
        packageName = "lost";
        version = "8.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/lost/-/lost-8.2.1.tgz";
          sha512 = "3wb4q0yvdi45jnw0zp3459q0583g5674mizsh88rb2rscllih0mm6dq0f783cb0v5n498z1f8250xyfl4hvj0b5d2722p5bia2kgr4b";
        };
      };
      "loud-rejection-1.6.0" = {
        name = "loud-rejection";
        packageName = "loud-rejection";
        version = "1.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/loud-rejection/-/loud-rejection-1.6.0.tgz";
          sha1 = "5b46f80147edee578870f086d04821cf998e551f";
        };
      };
      "lower-case-1.1.4" = {
        name = "lower-case";
        packageName = "lower-case";
        version = "1.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/lower-case/-/lower-case-1.1.4.tgz";
          sha1 = "9a2cabd1b9e8e0ae993a4bf7d5875c39c42e8eac";
        };
      };
      "lowercase-keys-1.0.0" = {
        name = "lowercase-keys";
        packageName = "lowercase-keys";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/lowercase-keys/-/lowercase-keys-1.0.0.tgz";
          sha1 = "4e3366b39e7f5457e35f1324bdf6f88d0bfc7306";
        };
      };
      "lru-cache-4.1.2" = {
        name = "lru-cache";
        packageName = "lru-cache";
        version = "4.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/lru-cache/-/lru-cache-4.1.2.tgz";
          sha512 = "1whynbvy3pbwcpkxk6rqhsymj2h3bh7p13nfhs9ch6hfx96vrh86j7vd4lqcaqjy5dhsfjps6sh2wqndh269wjz42khbh6339g9a1y2";
        };
      };
      "macaddress-0.2.8" = {
        name = "macaddress";
        packageName = "macaddress";
        version = "0.2.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/macaddress/-/macaddress-0.2.8.tgz";
          sha1 = "5904dc537c39ec6dbefeae902327135fa8511f12";
        };
      };
      "make-dir-1.2.0" = {
        name = "make-dir";
        packageName = "make-dir";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/make-dir/-/make-dir-1.2.0.tgz";
          sha512 = "0ivb7kryzyklvicp8a0lsq56pzjmvycb6bs4d0239q9ygcrs8ylx94q57fgxq3vqvzzs9v3ldl5m1jkxfvfaxh5p8lgb0qchmmh1mb8";
        };
      };
      "map-cache-0.2.2" = {
        name = "map-cache";
        packageName = "map-cache";
        version = "0.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/map-cache/-/map-cache-0.2.2.tgz";
          sha1 = "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf";
        };
      };
      "map-obj-1.0.1" = {
        name = "map-obj";
        packageName = "map-obj";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/map-obj/-/map-obj-1.0.1.tgz";
          sha1 = "d933ceb9205d82bdcf4886f6742bdc2b4dea146d";
        };
      };
      "map-visit-1.0.0" = {
        name = "map-visit";
        packageName = "map-visit";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/map-visit/-/map-visit-1.0.0.tgz";
          sha1 = "ecdca8f13144e660f1b5bd41f12f3479d98dfb8f";
        };
      };
      "matcher-0.1.2" = {
        name = "matcher";
        packageName = "matcher";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/matcher/-/matcher-0.1.2.tgz";
          sha1 = "ef20cbde64c24c50cc61af5b83ee0b1b8ff00101";
        };
      };
      "matcher-1.1.0" = {
        name = "matcher";
        packageName = "matcher";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/matcher/-/matcher-1.1.0.tgz";
          sha512 = "2agk4x4w1ibbn50ncyccd8azzmpwr8gwl3vbyr200fh6s6cy4ansrp5q49mf5y8czwzc8nz4hhc1abc4mh8wgfx07maf7jkj3laz4b9";
        };
      };
      "math-expression-evaluator-1.2.17" = {
        name = "math-expression-evaluator";
        packageName = "math-expression-evaluator";
        version = "1.2.17";
        src = fetchurl {
          url = "https://registry.npmjs.org/math-expression-evaluator/-/math-expression-evaluator-1.2.17.tgz";
          sha1 = "de819fdbcd84dccd8fae59c6aeb79615b9d266ac";
        };
      };
      "md5-hex-1.3.0" = {
        name = "md5-hex";
        packageName = "md5-hex";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/md5-hex/-/md5-hex-1.3.0.tgz";
          sha1 = "d2c4afe983c4370662179b8cad145219135046c4";
        };
      };
      "md5-hex-2.0.0" = {
        name = "md5-hex";
        packageName = "md5-hex";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/md5-hex/-/md5-hex-2.0.0.tgz";
          sha1 = "d0588e9f1c74954492ecd24ac0ac6ce997d92e33";
        };
      };
      "md5-o-matic-0.1.1" = {
        name = "md5-o-matic";
        packageName = "md5-o-matic";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/md5-o-matic/-/md5-o-matic-0.1.1.tgz";
          sha1 = "822bccd65e117c514fab176b25945d54100a03c3";
        };
      };
      "md5.js-1.3.4" = {
        name = "md5.js";
        packageName = "md5.js";
        version = "1.3.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/md5.js/-/md5.js-1.3.4.tgz";
          sha1 = "e9bdbde94a20a5ac18b04340fc5764d5b09d901d";
        };
      };
      "media-typer-0.3.0" = {
        name = "media-typer";
        packageName = "media-typer";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/media-typer/-/media-typer-0.3.0.tgz";
          sha1 = "8710d7af0aa626f8fffa1ce00168545263255748";
        };
      };
      "mem-1.1.0" = {
        name = "mem";
        packageName = "mem";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/mem/-/mem-1.1.0.tgz";
          sha1 = "5edd52b485ca1d900fe64895505399a0dfa45f76";
        };
      };
      "memory-fs-0.4.1" = {
        name = "memory-fs";
        packageName = "memory-fs";
        version = "0.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/memory-fs/-/memory-fs-0.4.1.tgz";
          sha1 = "3a9a20b8462523e447cfbc7e8bb80ed667bfc552";
        };
      };
      "meow-3.7.0" = {
        name = "meow";
        packageName = "meow";
        version = "3.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/meow/-/meow-3.7.0.tgz";
          sha1 = "72cb668b425228290abbfa856892587308a801fb";
        };
      };
      "merge-descriptors-1.0.1" = {
        name = "merge-descriptors";
        packageName = "merge-descriptors";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-1.0.1.tgz";
          sha1 = "b00aaa556dd8b44568150ec9d1b953f3f90cbb61";
        };
      };
      "methods-1.1.2" = {
        name = "methods";
        packageName = "methods";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz";
          sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
        };
      };
      "micromatch-2.3.11" = {
        name = "micromatch";
        packageName = "micromatch";
        version = "2.3.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/micromatch/-/micromatch-2.3.11.tgz";
          sha1 = "86677c97d1720b363431d04d0d15293bd38c1565";
        };
      };
      "micromatch-3.1.9" = {
        name = "micromatch";
        packageName = "micromatch";
        version = "3.1.9";
        src = fetchurl {
          url = "https://registry.npmjs.org/micromatch/-/micromatch-3.1.9.tgz";
          sha512 = "3vfciram7xbwcwvhii8960vnvf3jczn092sfbfgx1vivk3scvkm9znjnbq2ppywg9h8i86d8vq3ha4c9a3jllaian0gcl7rrgm36lja";
        };
      };
      "miller-rabin-4.0.1" = {
        name = "miller-rabin";
        packageName = "miller-rabin";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/miller-rabin/-/miller-rabin-4.0.1.tgz";
          sha512 = "12277knznlw4myxmgg6vgkrwmrhj9dyniscrlph3s08ndi2q25v3wrv6rwanvz29v5k5x756xa5yif4xllrghpn3jqaamnr3cp5ypnp";
        };
      };
      "mime-1.4.1" = {
        name = "mime";
        packageName = "mime";
        version = "1.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/mime/-/mime-1.4.1.tgz";
          sha512 = "2sz22r1xrnyvq6jg0h6b6cab3s3xdsfqa0n6vl9xv9gq3ppcxrcpg2hqfc41xjwnfwfkr6240l5gys7nds61ch6xcb3gr3fwsl7x398";
        };
      };
      "mime-1.6.0" = {
        name = "mime";
        packageName = "mime";
        version = "1.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz";
          sha512 = "1x901mk5cdib4xp27v4ivwwr7mhy64r4rk953bzivi5p9lf2bhw88ra2rhkd254xkdx2d3q30zkq239vc4yx4pfsj4hpys8rbr6fif7";
        };
      };
      "mime-db-1.33.0" = {
        name = "mime-db";
        packageName = "mime-db";
        version = "1.33.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/mime-db/-/mime-db-1.33.0.tgz";
          sha512 = "36xnw59ik9fqym00cmwb5nyzg0l03k70cp413f7639j93wgmzk1mh0xjc7i6zz3r6k9xnwh0g5cm5a1f3y8c6plgy4qld7fm887ywh4";
        };
      };
      "mime-types-2.1.18" = {
        name = "mime-types";
        packageName = "mime-types";
        version = "2.1.18";
        src = fetchurl {
          url = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.18.tgz";
          sha512 = "22krj1kw7n9z10zdyx7smcaim4bzwqsqzhspwha06q58gcrxfp93hw2cd0vk5crhq5p2dwzqlpacg32lrmp5sjzb798zdzy35mdmkwm";
        };
      };
      "mimic-fn-1.2.0" = {
        name = "mimic-fn";
        packageName = "mimic-fn";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/mimic-fn/-/mimic-fn-1.2.0.tgz";
          sha512 = "1clrrqw75rmpkw0x53axy8va2hq2gzynz9zb58m0xv1nh3lg81cv4dfdy7pk9vrlb9ydwmj6klpicwkv2bjx3m8aj4fi2ph3jxkizwd";
        };
      };
      "minimalistic-assert-1.0.0" = {
        name = "minimalistic-assert";
        packageName = "minimalistic-assert";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/minimalistic-assert/-/minimalistic-assert-1.0.0.tgz";
          sha1 = "702be2dda6b37f4836bcb3f5db56641b64a1d3d3";
        };
      };
      "minimalistic-crypto-utils-1.0.1" = {
        name = "minimalistic-crypto-utils";
        packageName = "minimalistic-crypto-utils";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/minimalistic-crypto-utils/-/minimalistic-crypto-utils-1.0.1.tgz";
          sha1 = "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a";
        };
      };
      "minimatch-3.0.4" = {
        name = "minimatch";
        packageName = "minimatch";
        version = "3.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/minimatch/-/minimatch-3.0.4.tgz";
          sha512 = "1879a3j85h92ypvb7lpv1dqpcxl49rqnbgs5la18zmj1yqhwl60c2m74254wbr5pp3znckqpkg9dvjyrz6hfz8b9vag5a3j910db4f8";
        };
      };
      "minimist-0.0.8" = {
        name = "minimist";
        packageName = "minimist";
        version = "0.0.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/minimist/-/minimist-0.0.8.tgz";
          sha1 = "857fcabfc3397d2625b8228262e86aa7a011b05d";
        };
      };
      "minimist-1.2.0" = {
        name = "minimist";
        packageName = "minimist";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/minimist/-/minimist-1.2.0.tgz";
          sha1 = "a35008b20f41383eec1fb914f4cd5df79a264284";
        };
      };
      "mississippi-2.0.0" = {
        name = "mississippi";
        packageName = "mississippi";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/mississippi/-/mississippi-2.0.0.tgz";
          sha512 = "2gnigixsj38hrqchkf6k8wqywihq6l14pg0qb23x3qpjn56vybljv3nz1z7cbzhl952h1xfw28vsqm09pjhpv9zs9sm43rdxazkqync";
        };
      };
      "mixin-deep-1.3.1" = {
        name = "mixin-deep";
        packageName = "mixin-deep";
        version = "1.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/mixin-deep/-/mixin-deep-1.3.1.tgz";
          sha512 = "0swcxf279pl5jc4b3d4fm9whbi6sbv2xbcfm1fkxiz793qa2hl5wd1b8dw019dgyidx5nwpa141k0cvvwan0rfypn2am0c4fwn2v4pi";
        };
      };
      "mkdirp-0.5.1" = {
        name = "mkdirp";
        packageName = "mkdirp";
        version = "0.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.1.tgz";
          sha1 = "30057438eac6cf7f8c4767f38648d6697d75c903";
        };
      };
      "module-alias-2.0.6" = {
        name = "module-alias";
        packageName = "module-alias";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/module-alias/-/module-alias-2.0.6.tgz";
          sha1 = "abb2cfa07014f503514ad5061c6f03d79b591889";
        };
      };
      "moment-2.21.0" = {
        name = "moment";
        packageName = "moment";
        version = "2.21.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/moment/-/moment-2.21.0.tgz";
          sha512 = "02hkdlhchyx4dqrh4q7p3c0xggpygcf0pimjvm66sib8z87wkbndici8vp2r39b472zpmybrf85y2c92lbw3hng9j2kfifl33l7f9jc";
        };
      };
      "move-concurrently-1.0.1" = {
        name = "move-concurrently";
        packageName = "move-concurrently";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/move-concurrently/-/move-concurrently-1.0.1.tgz";
          sha1 = "be2c005fda32e0b29af1f05d7c4b33214c701f92";
        };
      };
      "ms-1.0.0" = {
        name = "ms";
        packageName = "ms";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ms/-/ms-1.0.0.tgz";
          sha1 = "59adcd22edc543f7b5381862d31387b1f4bc9473";
        };
      };
      "ms-2.0.0" = {
        name = "ms";
        packageName = "ms";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ms/-/ms-2.0.0.tgz";
          sha1 = "5608aeadfc00be6c2901df5f9861788de0d597c8";
        };
      };
      "multicast-dns-6.2.3" = {
        name = "multicast-dns";
        packageName = "multicast-dns";
        version = "6.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/multicast-dns/-/multicast-dns-6.2.3.tgz";
          sha512 = "3m42kr8y2s1krl4gzb5xsa9v3h01xalvrgdkj99gyfaq7761asmj5m6kzm70mxb22125gia12g7rmarnzmh09403j8j2cyvx7jqjblf";
        };
      };
      "multicast-dns-service-types-1.1.0" = {
        name = "multicast-dns-service-types";
        packageName = "multicast-dns-service-types";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/multicast-dns-service-types/-/multicast-dns-service-types-1.1.0.tgz";
          sha1 = "899f11d9686e5e05cb91b35d5f0e63b773cfc901";
        };
      };
      "multimatch-2.1.0" = {
        name = "multimatch";
        packageName = "multimatch";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/multimatch/-/multimatch-2.1.0.tgz";
          sha1 = "9c7906a22fb4c02919e2f5f75161b4cdbd4b2a2b";
        };
      };
      "nan-2.9.2" = {
        name = "nan";
        packageName = "nan";
        version = "2.9.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/nan/-/nan-2.9.2.tgz";
          sha512 = "1iwlv98jbkr46c7hy28crk2m89jiskrp4qfa5ysyacyq8dkbk4hii3cc61irf39b7n6wd9cjlaasmakv9dsknqhb3876zrvrbjvmmcn";
        };
      };
      "nanomatch-1.2.9" = {
        name = "nanomatch";
        packageName = "nanomatch";
        version = "1.2.9";
        src = fetchurl {
          url = "https://registry.npmjs.org/nanomatch/-/nanomatch-1.2.9.tgz";
          sha512 = "1f2indb6hj1h79gxklk6ac2lhfjfydw5ak3v2nwilwir5hqvz9l9kxf63l8k1blsjq4v7r2pf0gqc1b55sqqym5bn9afhrj5xnpvi4z";
        };
      };
      "ncname-1.0.0" = {
        name = "ncname";
        packageName = "ncname";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ncname/-/ncname-1.0.0.tgz";
          sha1 = "5b57ad18b1ca092864ef62b0b1ed8194f383b71c";
        };
      };
      "negotiator-0.6.1" = {
        name = "negotiator";
        packageName = "negotiator";
        version = "0.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/negotiator/-/negotiator-0.6.1.tgz";
          sha1 = "2b327184e8992101177b28563fb5e7102acd0ca9";
        };
      };
      "neo-async-2.5.0" = {
        name = "neo-async";
        packageName = "neo-async";
        version = "2.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/neo-async/-/neo-async-2.5.0.tgz";
          sha512 = "3mgwi8gsgy9lazh0qbpaz8f2l8gvvpn3jp0ghc6xnn0xq0ajdmzp7lfklby1n4s67fy1w5g5gzizyqzds8l3fqsj76cy0mq06rr56cw";
        };
      };
      "no-case-2.3.2" = {
        name = "no-case";
        packageName = "no-case";
        version = "2.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/no-case/-/no-case-2.3.2.tgz";
          sha512 = "34msnfifpdmxl414b8rch1p1six59jd9251b7wkb37n78fa84xfa5f5f5cxxp477wb846nfrsg6b1py3rahz4xdpk17lzzy9kvdjr5f";
        };
      };
      "node-fetch-1.7.3" = {
        name = "node-fetch";
        packageName = "node-fetch";
        version = "1.7.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/node-fetch/-/node-fetch-1.7.3.tgz";
          sha512 = "0lz5m15w7qaks0a0s3dm0crsjrsd123dy00pn6qwcp50zfjykxkp22i5ymh6smlc0ags38nmdxlxw9yyq509azlv8kcdvdiq857h5in";
        };
      };
      "node-forge-0.7.1" = {
        name = "node-forge";
        packageName = "node-forge";
        version = "0.7.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/node-forge/-/node-forge-0.7.1.tgz";
          sha1 = "9da611ea08982f4b94206b3beb4cc9665f20c300";
        };
      };
      "node-libs-browser-2.1.0" = {
        name = "node-libs-browser";
        packageName = "node-libs-browser";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/node-libs-browser/-/node-libs-browser-2.1.0.tgz";
          sha512 = "05d8rzfa0aihb9s1i3fm0dmdvlspfrxf4pxnsd3nms75mviv86llgg2r30l7b38a9l93yb00k7dy1vs8h4nd30ihhyvyc88vb6wa374";
        };
      };
      "normalize-package-data-2.4.0" = {
        name = "normalize-package-data";
        packageName = "normalize-package-data";
        version = "2.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/normalize-package-data/-/normalize-package-data-2.4.0.tgz";
          sha512 = "01wzws79ps84ylshjb7rfpjykgiqxnpr89s52p2yyzfx8nfvyh5flvf1almiiavsi75xgi8g3s5davc1mmgz7gn8yvlqz6gnhax8f7n";
        };
      };
      "normalize-path-2.1.1" = {
        name = "normalize-path";
        packageName = "normalize-path";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/normalize-path/-/normalize-path-2.1.1.tgz";
          sha1 = "1ab28b556e198363a8c1a6f7e6fa20137fe6aed9";
        };
      };
      "normalize-range-0.1.2" = {
        name = "normalize-range";
        packageName = "normalize-range";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/normalize-range/-/normalize-range-0.1.2.tgz";
          sha1 = "2d10c06bdfd312ea9777695a4d28439456b75942";
        };
      };
      "normalize-url-1.9.1" = {
        name = "normalize-url";
        packageName = "normalize-url";
        version = "1.9.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/normalize-url/-/normalize-url-1.9.1.tgz";
          sha1 = "2cc0d66b31ea23036458436e3620d85954c66c3c";
        };
      };
      "npm-run-path-2.0.2" = {
        name = "npm-run-path";
        packageName = "npm-run-path";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/npm-run-path/-/npm-run-path-2.0.2.tgz";
          sha1 = "35a9232dfa35d7067b4cb2ddf2357b1871536c5f";
        };
      };
      "nth-check-1.0.1" = {
        name = "nth-check";
        packageName = "nth-check";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/nth-check/-/nth-check-1.0.1.tgz";
          sha1 = "9929acdf628fc2c41098deab82ac580cf149aae4";
        };
      };
      "num2fraction-1.2.2" = {
        name = "num2fraction";
        packageName = "num2fraction";
        version = "1.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/num2fraction/-/num2fraction-1.2.2.tgz";
          sha1 = "6f682b6a027a4e9ddfa4564cd2589d1d4e669ede";
        };
      };
      "number-is-nan-1.0.1" = {
        name = "number-is-nan";
        packageName = "number-is-nan";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/number-is-nan/-/number-is-nan-1.0.1.tgz";
          sha1 = "097b602b53422a522c1afb8790318336941a011d";
        };
      };
      "nwmatcher-1.4.3" = {
        name = "nwmatcher";
        packageName = "nwmatcher";
        version = "1.4.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/nwmatcher/-/nwmatcher-1.4.3.tgz";
          sha512 = "1dr5n7p33vykcsjrn9nxvmh26r6wh7is37cxcb28jsimj05kc33vi1bfd55gn3qymxi54cxj4ynvvnvbvpyblllv3gsl2fn417559r0";
        };
      };
      "oauth-sign-0.8.2" = {
        name = "oauth-sign";
        packageName = "oauth-sign";
        version = "0.8.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.8.2.tgz";
          sha1 = "46a6ab7f0aead8deae9ec0565780b7d4efeb9d43";
        };
      };
      "object-assign-4.1.1" = {
        name = "object-assign";
        packageName = "object-assign";
        version = "4.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz";
          sha1 = "2109adc7965887cfc05cbbd442cac8bfbb360863";
        };
      };
      "object-component-0.0.3" = {
        name = "object-component";
        packageName = "object-component";
        version = "0.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/object-component/-/object-component-0.0.3.tgz";
          sha1 = "f0c69aa50efc95b866c186f400a33769cb2f1291";
        };
      };
      "object-copy-0.1.0" = {
        name = "object-copy";
        packageName = "object-copy";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/object-copy/-/object-copy-0.1.0.tgz";
          sha1 = "7e7d858b781bd7c991a41ba975ed3812754e998c";
        };
      };
      "object-keys-1.0.11" = {
        name = "object-keys";
        packageName = "object-keys";
        version = "1.0.11";
        src = fetchurl {
          url = "https://registry.npmjs.org/object-keys/-/object-keys-1.0.11.tgz";
          sha1 = "c54601778ad560f1142ce0e01bcca8b56d13426d";
        };
      };
      "object-visit-1.0.1" = {
        name = "object-visit";
        packageName = "object-visit";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/object-visit/-/object-visit-1.0.1.tgz";
          sha1 = "f79c4493af0c5377b59fe39d395e41042dd045bb";
        };
      };
      "object.omit-2.0.1" = {
        name = "object.omit";
        packageName = "object.omit";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/object.omit/-/object.omit-2.0.1.tgz";
          sha1 = "1a9c744829f39dbb858c76ca3579ae2a54ebd1fa";
        };
      };
      "object.pick-1.3.0" = {
        name = "object.pick";
        packageName = "object.pick";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/object.pick/-/object.pick-1.3.0.tgz";
          sha1 = "87a10ac4c1694bd2e1cbf53591a66141fb5dd747";
        };
      };
      "observable-to-promise-0.5.0" = {
        name = "observable-to-promise";
        packageName = "observable-to-promise";
        version = "0.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/observable-to-promise/-/observable-to-promise-0.5.0.tgz";
          sha1 = "c828f0f0dc47e9f86af8a4977c5d55076ce7a91f";
        };
      };
      "obuf-1.1.1" = {
        name = "obuf";
        packageName = "obuf";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/obuf/-/obuf-1.1.1.tgz";
          sha1 = "104124b6c602c6796881a042541d36db43a5264e";
        };
      };
      "on-finished-2.3.0" = {
        name = "on-finished";
        packageName = "on-finished";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/on-finished/-/on-finished-2.3.0.tgz";
          sha1 = "20f1336481b083cd75337992a16971aa2d906947";
        };
      };
      "on-headers-1.0.1" = {
        name = "on-headers";
        packageName = "on-headers";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/on-headers/-/on-headers-1.0.1.tgz";
          sha1 = "928f5d0f470d49342651ea6794b0857c100693f7";
        };
      };
      "once-1.4.0" = {
        name = "once";
        packageName = "once";
        version = "1.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
          sha1 = "583b1aa775961d4b113ac17d9c50baef9dd76bd1";
        };
      };
      "onecolor-3.0.5" = {
        name = "onecolor";
        packageName = "onecolor";
        version = "3.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/onecolor/-/onecolor-3.0.5.tgz";
          sha1 = "36eff32201379efdf1180fb445e51a8e2425f9f6";
        };
      };
      "onetime-2.0.1" = {
        name = "onetime";
        packageName = "onetime";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/onetime/-/onetime-2.0.1.tgz";
          sha1 = "067428230fd67443b2794b22bba528b6867962d4";
        };
      };
      "opn-5.2.0" = {
        name = "opn";
        packageName = "opn";
        version = "5.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/opn/-/opn-5.2.0.tgz";
          sha512 = "12iyalgghs3dj0pfb7rxa013x946169yfsjfd15fsfrx5kv80z2qh082x7v7d91hh7bf9vxcm4wqmyyj9ckk3gnvc7mw77j6fkwdpr5";
        };
      };
      "option-chain-0.1.1" = {
        name = "option-chain";
        packageName = "option-chain";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/option-chain/-/option-chain-0.1.1.tgz";
          sha1 = "e9b811e006f1c0f54802f28295bfc8970f8dcfbd";
        };
      };
      "option-chain-1.0.0" = {
        name = "option-chain";
        packageName = "option-chain";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/option-chain/-/option-chain-1.0.0.tgz";
          sha1 = "938d73bd4e1783f948d34023644ada23669e30f2";
        };
      };
      "optionator-0.8.2" = {
        name = "optionator";
        packageName = "optionator";
        version = "0.8.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/optionator/-/optionator-0.8.2.tgz";
          sha1 = "364c5e409d3f4d6301d6c0b4c05bba50180aeb64";
        };
      };
      "original-1.0.0" = {
        name = "original";
        packageName = "original";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/original/-/original-1.0.0.tgz";
          sha1 = "9147f93fa1696d04be61e01bd50baeaca656bd3b";
        };
      };
      "os-browserify-0.3.0" = {
        name = "os-browserify";
        packageName = "os-browserify";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/os-browserify/-/os-browserify-0.3.0.tgz";
          sha1 = "854373c7f5c2315914fc9bfc6bd8238fdda1ec27";
        };
      };
      "os-homedir-1.0.2" = {
        name = "os-homedir";
        packageName = "os-homedir";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/os-homedir/-/os-homedir-1.0.2.tgz";
          sha1 = "ffbc4988336e0e833de0c168c7ef152121aa7fb3";
        };
      };
      "os-locale-1.4.0" = {
        name = "os-locale";
        packageName = "os-locale";
        version = "1.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/os-locale/-/os-locale-1.4.0.tgz";
          sha1 = "20f9f17ae29ed345e8bde583b13d2009803c14d9";
        };
      };
      "os-locale-2.1.0" = {
        name = "os-locale";
        packageName = "os-locale";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/os-locale/-/os-locale-2.1.0.tgz";
          sha512 = "0lafrp0i2ajapsnma0x74q7zscn97a56i5hh58a0nyig2igfx9fqn4ain9kvjrr06as5gzdrv2wdf52qc5m861fd0f4cv69ghdjbjyy";
        };
      };
      "os-tmpdir-1.0.2" = {
        name = "os-tmpdir";
        packageName = "os-tmpdir";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/os-tmpdir/-/os-tmpdir-1.0.2.tgz";
          sha1 = "bbe67406c79aa85c5cfec766fe5734555dfa1274";
        };
      };
      "p-finally-1.0.0" = {
        name = "p-finally";
        packageName = "p-finally";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/p-finally/-/p-finally-1.0.0.tgz";
          sha1 = "3fbcfb15b899a44123b34b6dcc18b724336a2cae";
        };
      };
      "p-limit-1.2.0" = {
        name = "p-limit";
        packageName = "p-limit";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/p-limit/-/p-limit-1.2.0.tgz";
          sha512 = "2g0r6r6bbcdp6lrxbj2zbcihr1byd55kycm1ijz80l2zvmcvhqsbd7rhmfqylp004d61fibvmwzk4ig89dbyk4azpwgll7dllhsvwv3";
        };
      };
      "p-locate-2.0.0" = {
        name = "p-locate";
        packageName = "p-locate";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/p-locate/-/p-locate-2.0.0.tgz";
          sha1 = "20a0103b222a70c8fd39cc2e580680f3dde5ec43";
        };
      };
      "p-map-1.2.0" = {
        name = "p-map";
        packageName = "p-map";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/p-map/-/p-map-1.2.0.tgz";
          sha512 = "084pyivsr35xi3fdmpznf0c0nc9jz15hak8iyh3v24n25b376blg13ngb4mgpm71zdnfp9b17zbyn728z0jjz1r674k71hd4c0cmb5g";
        };
      };
      "p-try-1.0.0" = {
        name = "p-try";
        packageName = "p-try";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/p-try/-/p-try-1.0.0.tgz";
          sha1 = "cbc79cdbaf8fd4228e13f621f2b1a237c1b207b3";
        };
      };
      "package-hash-1.2.0" = {
        name = "package-hash";
        packageName = "package-hash";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/package-hash/-/package-hash-1.2.0.tgz";
          sha1 = "003e56cd57b736a6ed6114cc2b81542672770e44";
        };
      };
      "package-hash-2.0.0" = {
        name = "package-hash";
        packageName = "package-hash";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/package-hash/-/package-hash-2.0.0.tgz";
          sha1 = "78ae326c89e05a4d813b68601977af05c00d2a0d";
        };
      };
      "package-json-4.0.1" = {
        name = "package-json";
        packageName = "package-json";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/package-json/-/package-json-4.0.1.tgz";
          sha1 = "8869a0401253661c4c4ca3da6c2121ed555f5eed";
        };
      };
      "pako-1.0.6" = {
        name = "pako";
        packageName = "pako";
        version = "1.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/pako/-/pako-1.0.6.tgz";
          sha512 = "1r9hy37qsbhv5ipsydkbir2yl7qg3lbpgj4qzrnb903w8mhj9ibaww0zykbp0ak1nxxp6mpbws3xsrf7fgq39zchci90c7chgqvh1wm";
        };
      };
      "parallel-transform-1.1.0" = {
        name = "parallel-transform";
        packageName = "parallel-transform";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/parallel-transform/-/parallel-transform-1.1.0.tgz";
          sha1 = "d410f065b05da23081fcd10f28854c29bda33b06";
        };
      };
      "param-case-2.1.1" = {
        name = "param-case";
        packageName = "param-case";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/param-case/-/param-case-2.1.1.tgz";
          sha1 = "df94fd8cf6531ecf75e6bef9a0858fbc72be2247";
        };
      };
      "parse-asn1-5.1.0" = {
        name = "parse-asn1";
        packageName = "parse-asn1";
        version = "5.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-asn1/-/parse-asn1-5.1.0.tgz";
          sha1 = "37c4f9b7ed3ab65c74817b5f2480937fbf97c712";
        };
      };
      "parse-glob-3.0.4" = {
        name = "parse-glob";
        packageName = "parse-glob";
        version = "3.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-glob/-/parse-glob-3.0.4.tgz";
          sha1 = "b2c376cfb11f35513badd173ef0bb6e3a388391c";
        };
      };
      "parse-json-2.2.0" = {
        name = "parse-json";
        packageName = "parse-json";
        version = "2.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-json/-/parse-json-2.2.0.tgz";
          sha1 = "f480f40434ef80741f8469099f8dea18f55a4dc9";
        };
      };
      "parse-json-4.0.0" = {
        name = "parse-json";
        packageName = "parse-json";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-json/-/parse-json-4.0.0.tgz";
          sha1 = "be35f5425be1f7f6c747184f98a788cb99477ee0";
        };
      };
      "parse-ms-0.1.2" = {
        name = "parse-ms";
        packageName = "parse-ms";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-ms/-/parse-ms-0.1.2.tgz";
          sha1 = "dd3fa25ed6c2efc7bdde12ad9b46c163aa29224e";
        };
      };
      "parse-ms-1.0.1" = {
        name = "parse-ms";
        packageName = "parse-ms";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse-ms/-/parse-ms-1.0.1.tgz";
          sha1 = "56346d4749d78f23430ca0c713850aef91aa361d";
        };
      };
      "parse5-4.0.0" = {
        name = "parse5";
        packageName = "parse5";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/parse5/-/parse5-4.0.0.tgz";
          sha512 = "0a10cnny1cy9csxrw2736fjbhqp0jqv8hnghr7gz9jrlkclw91f8rzg1173b43b3dbc1qq68g7vy4hq4b45r3bmw5j52kvpwxw7pdjn";
        };
      };
      "parseqs-0.0.5" = {
        name = "parseqs";
        packageName = "parseqs";
        version = "0.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/parseqs/-/parseqs-0.0.5.tgz";
          sha1 = "d5208a3738e46766e291ba2ea173684921a8b89d";
        };
      };
      "parseuri-0.0.5" = {
        name = "parseuri";
        packageName = "parseuri";
        version = "0.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/parseuri/-/parseuri-0.0.5.tgz";
          sha1 = "80204a50d4dbb779bfdc6ebe2778d90e4bce320a";
        };
      };
      "parseurl-1.3.2" = {
        name = "parseurl";
        packageName = "parseurl";
        version = "1.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/parseurl/-/parseurl-1.3.2.tgz";
          sha1 = "fc289d4ed8993119460c156253262cdc8de65bf3";
        };
      };
      "pascalcase-0.1.1" = {
        name = "pascalcase";
        packageName = "pascalcase";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/pascalcase/-/pascalcase-0.1.1.tgz";
          sha1 = "b363e55e8006ca6fe21784d2db22bd15d7917f14";
        };
      };
      "path-browserify-0.0.0" = {
        name = "path-browserify";
        packageName = "path-browserify";
        version = "0.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-browserify/-/path-browserify-0.0.0.tgz";
          sha1 = "a0b870729aae214005b7d5032ec2cbbb0fb4451a";
        };
      };
      "path-dirname-1.0.2" = {
        name = "path-dirname";
        packageName = "path-dirname";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-dirname/-/path-dirname-1.0.2.tgz";
          sha1 = "cc33d24d525e099a5388c0336c6e32b9160609e0";
        };
      };
      "path-exists-2.1.0" = {
        name = "path-exists";
        packageName = "path-exists";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-exists/-/path-exists-2.1.0.tgz";
          sha1 = "0feb6c64f0fc518d9a754dd5efb62c7022761f4b";
        };
      };
      "path-exists-3.0.0" = {
        name = "path-exists";
        packageName = "path-exists";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-exists/-/path-exists-3.0.0.tgz";
          sha1 = "ce0ebeaa5f78cb18925ea7d810d7b59b010fd515";
        };
      };
      "path-is-absolute-1.0.1" = {
        name = "path-is-absolute";
        packageName = "path-is-absolute";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
          sha1 = "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f";
        };
      };
      "path-is-inside-1.0.2" = {
        name = "path-is-inside";
        packageName = "path-is-inside";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-is-inside/-/path-is-inside-1.0.2.tgz";
          sha1 = "365417dede44430d1c11af61027facf074bdfc53";
        };
      };
      "path-key-2.0.1" = {
        name = "path-key";
        packageName = "path-key";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-key/-/path-key-2.0.1.tgz";
          sha1 = "411cadb574c5a140d3a4b1910d40d80cc9f40b40";
        };
      };
      "path-parse-1.0.5" = {
        name = "path-parse";
        packageName = "path-parse";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-parse/-/path-parse-1.0.5.tgz";
          sha1 = "3c1adf871ea9cd6c9431b6ea2bd74a0ff055c4c1";
        };
      };
      "path-to-regexp-0.1.7" = {
        name = "path-to-regexp";
        packageName = "path-to-regexp";
        version = "0.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-0.1.7.tgz";
          sha1 = "df604178005f522f15eb4490e7247a1bfaa67f8c";
        };
      };
      "path-type-1.1.0" = {
        name = "path-type";
        packageName = "path-type";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-type/-/path-type-1.1.0.tgz";
          sha1 = "59c44f7ee491da704da415da5a4070ba4f8fe441";
        };
      };
      "path-type-2.0.0" = {
        name = "path-type";
        packageName = "path-type";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-type/-/path-type-2.0.0.tgz";
          sha1 = "f012ccb8415b7096fc2daa1054c3d72389594c73";
        };
      };
      "path-type-3.0.0" = {
        name = "path-type";
        packageName = "path-type";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/path-type/-/path-type-3.0.0.tgz";
          sha512 = "2z1csf4c3fmlwl0ahk533z5zqkjdf36ccfx11kakl9xran9f5asxm4cxjq4lx1kwqdp8gki786cgpldvgrkvfc7pcvh07j5ssqm8rjg";
        };
      };
      "pbkdf2-3.0.14" = {
        name = "pbkdf2";
        packageName = "pbkdf2";
        version = "3.0.14";
        src = fetchurl {
          url = "https://registry.npmjs.org/pbkdf2/-/pbkdf2-3.0.14.tgz";
          sha512 = "30bb7vx0m1k1m3d1i1khgvmgddx3ahqgprs421ssrh5plpx50k5bazsj67gdi7qiknircqy59yxbclq95s2rnmk8ysgkqdpsddijfw2";
        };
      };
      "performance-now-2.1.0" = {
        name = "performance-now";
        packageName = "performance-now";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/performance-now/-/performance-now-2.1.0.tgz";
          sha1 = "6309f4e0e5fa913ec1c69307ae364b4b377c9e7b";
        };
      };
      "pify-2.3.0" = {
        name = "pify";
        packageName = "pify";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pify/-/pify-2.3.0.tgz";
          sha1 = "ed141a6ac043a849ea588498e7dca8b15330e90c";
        };
      };
      "pify-3.0.0" = {
        name = "pify";
        packageName = "pify";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pify/-/pify-3.0.0.tgz";
          sha1 = "e5a4acd2c101fdf3d9a4d07f0dbc4db49dd28176";
        };
      };
      "pinkie-1.0.0" = {
        name = "pinkie";
        packageName = "pinkie";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pinkie/-/pinkie-1.0.0.tgz";
          sha1 = "5a47f28ba1015d0201bda7bf0f358e47bec8c7e4";
        };
      };
      "pinkie-2.0.4" = {
        name = "pinkie";
        packageName = "pinkie";
        version = "2.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/pinkie/-/pinkie-2.0.4.tgz";
          sha1 = "72556b80cfa0d48a974e80e77248e80ed4f7f870";
        };
      };
      "pinkie-promise-1.0.0" = {
        name = "pinkie-promise";
        packageName = "pinkie-promise";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pinkie-promise/-/pinkie-promise-1.0.0.tgz";
          sha1 = "d1da67f5482563bb7cf57f286ae2822ecfbf3670";
        };
      };
      "pinkie-promise-2.0.1" = {
        name = "pinkie-promise";
        packageName = "pinkie-promise";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/pinkie-promise/-/pinkie-promise-2.0.1.tgz";
          sha1 = "2135d6dfa7a358c069ac9b178776288228450ffa";
        };
      };
      "pixrem-4.0.1" = {
        name = "pixrem";
        packageName = "pixrem";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/pixrem/-/pixrem-4.0.1.tgz";
          sha1 = "2da4a1de6ec4423c5fc3794e930b81d4490ec686";
        };
      };
      "pkg-conf-2.1.0" = {
        name = "pkg-conf";
        packageName = "pkg-conf";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pkg-conf/-/pkg-conf-2.1.0.tgz";
          sha1 = "2126514ca6f2abfebd168596df18ba57867f0058";
        };
      };
      "pkg-dir-1.0.0" = {
        name = "pkg-dir";
        packageName = "pkg-dir";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pkg-dir/-/pkg-dir-1.0.0.tgz";
          sha1 = "7a4b508a8d5bb2d629d447056ff4e9c9314cf3d4";
        };
      };
      "pkg-dir-2.0.0" = {
        name = "pkg-dir";
        packageName = "pkg-dir";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pkg-dir/-/pkg-dir-2.0.0.tgz";
          sha1 = "f6d5d1109e19d63edf428e0bd57e12777615334b";
        };
      };
      "pleeease-filters-4.0.0" = {
        name = "pleeease-filters";
        packageName = "pleeease-filters";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pleeease-filters/-/pleeease-filters-4.0.0.tgz";
          sha1 = "6632b2fb05648d2758d865384fbced79e1ccaec7";
        };
      };
      "plur-1.0.0" = {
        name = "plur";
        packageName = "plur";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/plur/-/plur-1.0.0.tgz";
          sha1 = "db85c6814f5e5e5a3b49efc28d604fec62975156";
        };
      };
      "plur-2.1.2" = {
        name = "plur";
        packageName = "plur";
        version = "2.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/plur/-/plur-2.1.2.tgz";
          sha1 = "7482452c1a0f508e3e344eaec312c91c29dc655a";
        };
      };
      "pn-1.1.0" = {
        name = "pn";
        packageName = "pn";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pn/-/pn-1.1.0.tgz";
          sha512 = "2fgy9i99qaapk6lrqsf0i396i0hw5prvpn2wwsccs7j9jz8960r59vjz98kjhfiif2pyf4fiizrwpminc29rdhpld2v8m7n18hxm8fs";
        };
      };
      "portfinder-1.0.13" = {
        name = "portfinder";
        packageName = "portfinder";
        version = "1.0.13";
        src = fetchurl {
          url = "https://registry.npmjs.org/portfinder/-/portfinder-1.0.13.tgz";
          sha1 = "bb32ecd87c27104ae6ee44b5a3ccbf0ebb1aede9";
        };
      };
      "posix-character-classes-0.1.1" = {
        name = "posix-character-classes";
        packageName = "posix-character-classes";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/posix-character-classes/-/posix-character-classes-0.1.1.tgz";
          sha1 = "01eac0fe3b5af71a2a6c02feabb8c1fef7e00eab";
        };
      };
      "postcss-5.2.18" = {
        name = "postcss";
        packageName = "postcss";
        version = "5.2.18";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss/-/postcss-5.2.18.tgz";
          sha512 = "1gfpqwz21lbmjgbgx46nzh7fddyrdlsl30rkirsc1d6nih1s971p2vp5vpc4srl64j23fym9a45b9lf969h0skxn759avmm2x2j7dff";
        };
      };
      "postcss-6.0.19" = {
        name = "postcss";
        packageName = "postcss";
        version = "6.0.19";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss/-/postcss-6.0.19.tgz";
          sha512 = "174sg3cs8v8bqg8rnk673qp365n46kls3f3a41pp0jx48qivkg06rck0j2bfyzm5hr1i6kjbcn82h1rkjgfi5jbd0amrd877m3wfpbz";
        };
      };
      "postcss-apply-0.8.0" = {
        name = "postcss-apply";
        packageName = "postcss-apply";
        version = "0.8.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-apply/-/postcss-apply-0.8.0.tgz";
          sha1 = "14e544bbb5cb6f1c1e048857965d79ae066b1343";
        };
      };
      "postcss-attribute-case-insensitive-2.0.0" = {
        name = "postcss-attribute-case-insensitive";
        packageName = "postcss-attribute-case-insensitive";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-attribute-case-insensitive/-/postcss-attribute-case-insensitive-2.0.0.tgz";
          sha1 = "94dc422c8f90997f16bd33a3654bbbec084963b4";
        };
      };
      "postcss-button-0.3.7" = {
        name = "postcss-button";
        packageName = "postcss-button";
        version = "0.3.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-button/-/postcss-button-0.3.7.tgz";
          sha512 = "0vpcvpxyh75rinhj1kqzxma0vx585y7r6r5vqd2xr3kr20syfzqxpayaqnk3qcr7vbgscf68vhwizc6d5ac1j9113zr5vacn4dcixr0";
        };
      };
      "postcss-calc-5.3.1" = {
        name = "postcss-calc";
        packageName = "postcss-calc";
        version = "5.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-calc/-/postcss-calc-5.3.1.tgz";
          sha1 = "77bae7ca928ad85716e2fda42f261bf7c1d65b5e";
        };
      };
      "postcss-calc-6.0.1" = {
        name = "postcss-calc";
        packageName = "postcss-calc";
        version = "6.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-calc/-/postcss-calc-6.0.1.tgz";
          sha1 = "3d24171bbf6e7629d422a436ebfe6dd9511f4330";
        };
      };
      "postcss-color-function-4.0.1" = {
        name = "postcss-color-function";
        packageName = "postcss-color-function";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-function/-/postcss-color-function-4.0.1.tgz";
          sha1 = "402b3f2cebc3f6947e618fb6be3654fbecef6444";
        };
      };
      "postcss-color-gray-4.1.0" = {
        name = "postcss-color-gray";
        packageName = "postcss-color-gray";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-gray/-/postcss-color-gray-4.1.0.tgz";
          sha512 = "3xhs4fw7kwsmai87d6parynp8rqi03yn82gcmqvh6g32351vcp1x6m5qy5dcwv04qf5c97w1qvh26a7knz1k84kx5rjia6x08lqp21g";
        };
      };
      "postcss-color-hex-alpha-3.0.0" = {
        name = "postcss-color-hex-alpha";
        packageName = "postcss-color-hex-alpha";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-hex-alpha/-/postcss-color-hex-alpha-3.0.0.tgz";
          sha1 = "1e53e6c8acb237955e8fd08b7ecdb1b8b8309f95";
        };
      };
      "postcss-color-hsl-2.0.0" = {
        name = "postcss-color-hsl";
        packageName = "postcss-color-hsl";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-hsl/-/postcss-color-hsl-2.0.0.tgz";
          sha1 = "12703666fa310430e3f30a454dac1386317d5844";
        };
      };
      "postcss-color-hwb-3.0.0" = {
        name = "postcss-color-hwb";
        packageName = "postcss-color-hwb";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-hwb/-/postcss-color-hwb-3.0.0.tgz";
          sha1 = "3402b19ef4d8497540c1fb5072be9863ca95571e";
        };
      };
      "postcss-color-rebeccapurple-3.0.0" = {
        name = "postcss-color-rebeccapurple";
        packageName = "postcss-color-rebeccapurple";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-rebeccapurple/-/postcss-color-rebeccapurple-3.0.0.tgz";
          sha1 = "eebaf03d363b4300b96792bd3081c19ed66513d3";
        };
      };
      "postcss-color-rgb-2.0.0" = {
        name = "postcss-color-rgb";
        packageName = "postcss-color-rgb";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-rgb/-/postcss-color-rgb-2.0.0.tgz";
          sha1 = "14539c8a7131494b482e0dd1cc265ff6514b5263";
        };
      };
      "postcss-color-rgba-fallback-3.0.0" = {
        name = "postcss-color-rgba-fallback";
        packageName = "postcss-color-rgba-fallback";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-color-rgba-fallback/-/postcss-color-rgba-fallback-3.0.0.tgz";
          sha1 = "37d5c9353a07a09270912a82606bb42a0d702c04";
        };
      };
      "postcss-colormin-2.2.2" = {
        name = "postcss-colormin";
        packageName = "postcss-colormin";
        version = "2.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-colormin/-/postcss-colormin-2.2.2.tgz";
          sha1 = "6631417d5f0e909a3d7ec26b24c8a8d1e4f96e4b";
        };
      };
      "postcss-convert-values-2.6.1" = {
        name = "postcss-convert-values";
        packageName = "postcss-convert-values";
        version = "2.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-convert-values/-/postcss-convert-values-2.6.1.tgz";
          sha1 = "bbd8593c5c1fd2e3d1c322bb925dcae8dae4d62d";
        };
      };
      "postcss-css-reset-1.0.2" = {
        name = "postcss-css-reset";
        packageName = "postcss-css-reset";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-css-reset/-/postcss-css-reset-1.0.2.tgz";
          sha1 = "769f2d62d3f1f76e2600fb4f79066399bebe2bd8";
        };
      };
      "postcss-cssnext-3.1.0" = {
        name = "postcss-cssnext";
        packageName = "postcss-cssnext";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-cssnext/-/postcss-cssnext-3.1.0.tgz";
          sha512 = "2s1grks9wgvy5ng3k3yjcjhp3ky4x07wsyy33cl1sj9wj81z4rckaq1x5ipaaj0x0mpvgmzlwzyx00ajm4fpy9a3iffna8fis2c60vb";
        };
      };
      "postcss-custom-media-6.0.0" = {
        name = "postcss-custom-media";
        packageName = "postcss-custom-media";
        version = "6.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-custom-media/-/postcss-custom-media-6.0.0.tgz";
          sha1 = "be532784110ecb295044fb5395a18006eb21a737";
        };
      };
      "postcss-custom-properties-6.3.1" = {
        name = "postcss-custom-properties";
        packageName = "postcss-custom-properties";
        version = "6.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-custom-properties/-/postcss-custom-properties-6.3.1.tgz";
          sha512 = "10vc0w5a8n1dnrmgrwv68xjgnarf7mp96mxkf3llvdcwkrhwvadjl85ragxdcxzxjw6x8jh0pm12n9cb71q3742mxdl3282ifgv126f";
        };
      };
      "postcss-custom-selectors-4.0.1" = {
        name = "postcss-custom-selectors";
        packageName = "postcss-custom-selectors";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-custom-selectors/-/postcss-custom-selectors-4.0.1.tgz";
          sha1 = "781382f94c52e727ef5ca4776ea2adf49a611382";
        };
      };
      "postcss-discard-comments-2.0.4" = {
        name = "postcss-discard-comments";
        packageName = "postcss-discard-comments";
        version = "2.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-discard-comments/-/postcss-discard-comments-2.0.4.tgz";
          sha1 = "befe89fafd5b3dace5ccce51b76b81514be00e3d";
        };
      };
      "postcss-discard-duplicates-2.1.0" = {
        name = "postcss-discard-duplicates";
        packageName = "postcss-discard-duplicates";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-discard-duplicates/-/postcss-discard-duplicates-2.1.0.tgz";
          sha1 = "b9abf27b88ac188158a5eb12abcae20263b91932";
        };
      };
      "postcss-discard-empty-2.1.0" = {
        name = "postcss-discard-empty";
        packageName = "postcss-discard-empty";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-discard-empty/-/postcss-discard-empty-2.1.0.tgz";
          sha1 = "d2b4bd9d5ced5ebd8dcade7640c7d7cd7f4f92b5";
        };
      };
      "postcss-discard-overridden-0.1.1" = {
        name = "postcss-discard-overridden";
        packageName = "postcss-discard-overridden";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-discard-overridden/-/postcss-discard-overridden-0.1.1.tgz";
          sha1 = "8b1eaf554f686fb288cd874c55667b0aa3668d58";
        };
      };
      "postcss-discard-unused-2.2.3" = {
        name = "postcss-discard-unused";
        packageName = "postcss-discard-unused";
        version = "2.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-discard-unused/-/postcss-discard-unused-2.2.3.tgz";
          sha1 = "bce30b2cc591ffc634322b5fb3464b6d934f4433";
        };
      };
      "postcss-extend-1.0.5" = {
        name = "postcss-extend";
        packageName = "postcss-extend";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-extend/-/postcss-extend-1.0.5.tgz";
          sha1 = "5ea98bf787ba3cacf4df4609743f80a833b1d0e7";
        };
      };
      "postcss-filter-plugins-2.0.2" = {
        name = "postcss-filter-plugins";
        packageName = "postcss-filter-plugins";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-filter-plugins/-/postcss-filter-plugins-2.0.2.tgz";
          sha1 = "6d85862534d735ac420e4a85806e1f5d4286d84c";
        };
      };
      "postcss-flexbox-1.0.3" = {
        name = "postcss-flexbox";
        packageName = "postcss-flexbox";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-flexbox/-/postcss-flexbox-1.0.3.tgz";
          sha1 = "2f255c7857d693cf6cfe42567ea4382dad9e70c5";
        };
      };
      "postcss-font-family-system-ui-3.0.0" = {
        name = "postcss-font-family-system-ui";
        packageName = "postcss-font-family-system-ui";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-font-family-system-ui/-/postcss-font-family-system-ui-3.0.0.tgz";
          sha512 = "11nbnqlk9yzhq6zry430fvcbpaj394qlbxlc7il88gqc3dq64jrzg85zgs2zf397cazgkh4cdbs33nawm43sp0s46jj4jac7j2vzhg7";
        };
      };
      "postcss-font-variant-3.0.0" = {
        name = "postcss-font-variant";
        packageName = "postcss-font-variant";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-font-variant/-/postcss-font-variant-3.0.0.tgz";
          sha1 = "08ccc88f6050ba82ed8ef2cc76c0c6a6b41f183e";
        };
      };
      "postcss-image-set-polyfill-0.3.5" = {
        name = "postcss-image-set-polyfill";
        packageName = "postcss-image-set-polyfill";
        version = "0.3.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-image-set-polyfill/-/postcss-image-set-polyfill-0.3.5.tgz";
          sha1 = "0f193413700cf1f82bd39066ef016d65a4a18181";
        };
      };
      "postcss-import-11.1.0" = {
        name = "postcss-import";
        packageName = "postcss-import";
        version = "11.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-import/-/postcss-import-11.1.0.tgz";
          sha512 = "05kyiqgx640fs08qhbp4znvj9ccicipwvv70xrcyx8ya488kzbdna2cr8a15k7bliw1qx60bh1bwi09a43mwr1wkslg7r1v4bpgcpg6";
        };
      };
      "postcss-initial-2.0.0" = {
        name = "postcss-initial";
        packageName = "postcss-initial";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-initial/-/postcss-initial-2.0.0.tgz";
          sha1 = "72715f7336e0bb79351d99ee65c4a253a8441ba4";
        };
      };
      "postcss-inline-svg-3.0.0" = {
        name = "postcss-inline-svg";
        packageName = "postcss-inline-svg";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-inline-svg/-/postcss-inline-svg-3.0.0.tgz";
          sha1 = "195da9f5487bf8f183b03f8a2a843b060ff00525";
        };
      };
      "postcss-load-config-1.2.0" = {
        name = "postcss-load-config";
        packageName = "postcss-load-config";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-load-config/-/postcss-load-config-1.2.0.tgz";
          sha1 = "539e9afc9ddc8620121ebf9d8c3673e0ce50d28a";
        };
      };
      "postcss-load-options-1.2.0" = {
        name = "postcss-load-options";
        packageName = "postcss-load-options";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-load-options/-/postcss-load-options-1.2.0.tgz";
          sha1 = "b098b1559ddac2df04bc0bb375f99a5cfe2b6d8c";
        };
      };
      "postcss-load-plugins-2.3.0" = {
        name = "postcss-load-plugins";
        packageName = "postcss-load-plugins";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-load-plugins/-/postcss-load-plugins-2.3.0.tgz";
          sha1 = "745768116599aca2f009fad426b00175049d8d92";
        };
      };
      "postcss-loader-2.1.1" = {
        name = "postcss-loader";
        packageName = "postcss-loader";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-loader/-/postcss-loader-2.1.1.tgz";
          sha512 = "3zj7mn0kwsynf6nlv812lrgc5p921ww3h7hds012b46bv239dcwb6amdl84jxbs3fdc9wmpkmjvyz4jaxx5ix5izjyj71rzc46pyhkz";
        };
      };
      "postcss-media-minmax-3.0.0" = {
        name = "postcss-media-minmax";
        packageName = "postcss-media-minmax";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-media-minmax/-/postcss-media-minmax-3.0.0.tgz";
          sha1 = "675256037a43ef40bc4f0760bfd06d4dc69d48d2";
        };
      };
      "postcss-media-query-parser-0.2.3" = {
        name = "postcss-media-query-parser";
        packageName = "postcss-media-query-parser";
        version = "0.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-media-query-parser/-/postcss-media-query-parser-0.2.3.tgz";
          sha1 = "27b39c6f4d94f81b1a73b8f76351c609e5cef244";
        };
      };
      "postcss-merge-idents-2.1.7" = {
        name = "postcss-merge-idents";
        packageName = "postcss-merge-idents";
        version = "2.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-merge-idents/-/postcss-merge-idents-2.1.7.tgz";
          sha1 = "4c5530313c08e1d5b3bbf3d2bbc747e278eea270";
        };
      };
      "postcss-merge-longhand-2.0.2" = {
        name = "postcss-merge-longhand";
        packageName = "postcss-merge-longhand";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-merge-longhand/-/postcss-merge-longhand-2.0.2.tgz";
          sha1 = "23d90cd127b0a77994915332739034a1a4f3d658";
        };
      };
      "postcss-merge-rules-2.1.2" = {
        name = "postcss-merge-rules";
        packageName = "postcss-merge-rules";
        version = "2.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-merge-rules/-/postcss-merge-rules-2.1.2.tgz";
          sha1 = "d1df5dfaa7b1acc3be553f0e9e10e87c61b5f721";
        };
      };
      "postcss-message-helpers-2.0.0" = {
        name = "postcss-message-helpers";
        packageName = "postcss-message-helpers";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-message-helpers/-/postcss-message-helpers-2.0.0.tgz";
          sha1 = "a4f2f4fab6e4fe002f0aed000478cdf52f9ba60e";
        };
      };
      "postcss-minify-font-values-1.0.5" = {
        name = "postcss-minify-font-values";
        packageName = "postcss-minify-font-values";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-minify-font-values/-/postcss-minify-font-values-1.0.5.tgz";
          sha1 = "4b58edb56641eba7c8474ab3526cafd7bbdecb69";
        };
      };
      "postcss-minify-gradients-1.0.5" = {
        name = "postcss-minify-gradients";
        packageName = "postcss-minify-gradients";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-minify-gradients/-/postcss-minify-gradients-1.0.5.tgz";
          sha1 = "5dbda11373703f83cfb4a3ea3881d8d75ff5e6e1";
        };
      };
      "postcss-minify-params-1.2.2" = {
        name = "postcss-minify-params";
        packageName = "postcss-minify-params";
        version = "1.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-minify-params/-/postcss-minify-params-1.2.2.tgz";
          sha1 = "ad2ce071373b943b3d930a3fa59a358c28d6f1f3";
        };
      };
      "postcss-minify-selectors-2.1.1" = {
        name = "postcss-minify-selectors";
        packageName = "postcss-minify-selectors";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-minify-selectors/-/postcss-minify-selectors-2.1.1.tgz";
          sha1 = "b2c6a98c0072cf91b932d1a496508114311735bf";
        };
      };
      "postcss-modules-extract-imports-1.2.0" = {
        name = "postcss-modules-extract-imports";
        packageName = "postcss-modules-extract-imports";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-modules-extract-imports/-/postcss-modules-extract-imports-1.2.0.tgz";
          sha1 = "66140ecece38ef06bf0d3e355d69bf59d141ea85";
        };
      };
      "postcss-modules-local-by-default-1.2.0" = {
        name = "postcss-modules-local-by-default";
        packageName = "postcss-modules-local-by-default";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-modules-local-by-default/-/postcss-modules-local-by-default-1.2.0.tgz";
          sha1 = "f7d80c398c5a393fa7964466bd19500a7d61c069";
        };
      };
      "postcss-modules-scope-1.1.0" = {
        name = "postcss-modules-scope";
        packageName = "postcss-modules-scope";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-modules-scope/-/postcss-modules-scope-1.1.0.tgz";
          sha1 = "d6ea64994c79f97b62a72b426fbe6056a194bb90";
        };
      };
      "postcss-modules-values-1.3.0" = {
        name = "postcss-modules-values";
        packageName = "postcss-modules-values";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-modules-values/-/postcss-modules-values-1.3.0.tgz";
          sha1 = "ecffa9d7e192518389f42ad0e83f72aec456ea20";
        };
      };
      "postcss-nested-2.1.2" = {
        name = "postcss-nested";
        packageName = "postcss-nested";
        version = "2.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-nested/-/postcss-nested-2.1.2.tgz";
          sha512 = "1gba9jy8daklk1rhkxz7naxxvh9mvz5vvychip848x0059yyzfvfkpsvwmjn0m9yd5lh253s7dy6x14ib7rfaswdimj6rafn66wlkh9";
        };
      };
      "postcss-nesting-4.2.1" = {
        name = "postcss-nesting";
        packageName = "postcss-nesting";
        version = "4.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-nesting/-/postcss-nesting-4.2.1.tgz";
          sha512 = "1l46r8k1l54b0a2kmmxs8b7bh2kw5lv74qsh2p2lkr5l0kildax3nyfcv61lpq1iraibyrpzk09v8zfcbn4bqmbjnkh0smhh1f9ck12";
        };
      };
      "postcss-normalize-charset-1.1.1" = {
        name = "postcss-normalize-charset";
        packageName = "postcss-normalize-charset";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-normalize-charset/-/postcss-normalize-charset-1.1.1.tgz";
          sha1 = "ef9ee71212d7fe759c78ed162f61ed62b5cb93f1";
        };
      };
      "postcss-normalize-url-3.0.8" = {
        name = "postcss-normalize-url";
        packageName = "postcss-normalize-url";
        version = "3.0.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-normalize-url/-/postcss-normalize-url-3.0.8.tgz";
          sha1 = "108f74b3f2fcdaf891a2ffa3ea4592279fc78222";
        };
      };
      "postcss-ordered-values-2.2.3" = {
        name = "postcss-ordered-values";
        packageName = "postcss-ordered-values";
        version = "2.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-ordered-values/-/postcss-ordered-values-2.2.3.tgz";
          sha1 = "eec6c2a67b6c412a8db2042e77fe8da43f95c11d";
        };
      };
      "postcss-pseudo-class-any-link-4.0.0" = {
        name = "postcss-pseudo-class-any-link";
        packageName = "postcss-pseudo-class-any-link";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-pseudo-class-any-link/-/postcss-pseudo-class-any-link-4.0.0.tgz";
          sha1 = "9152a0613d3450720513e8892854bae42d0ee68e";
        };
      };
      "postcss-pseudoelements-5.0.0" = {
        name = "postcss-pseudoelements";
        packageName = "postcss-pseudoelements";
        version = "5.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-pseudoelements/-/postcss-pseudoelements-5.0.0.tgz";
          sha1 = "eef194e8d524645ca520a949e95e518e812402cb";
        };
      };
      "postcss-reduce-idents-2.4.0" = {
        name = "postcss-reduce-idents";
        packageName = "postcss-reduce-idents";
        version = "2.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-reduce-idents/-/postcss-reduce-idents-2.4.0.tgz";
          sha1 = "c2c6d20cc958284f6abfbe63f7609bf409059ad3";
        };
      };
      "postcss-reduce-initial-1.0.1" = {
        name = "postcss-reduce-initial";
        packageName = "postcss-reduce-initial";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-reduce-initial/-/postcss-reduce-initial-1.0.1.tgz";
          sha1 = "68f80695f045d08263a879ad240df8dd64f644ea";
        };
      };
      "postcss-reduce-transforms-1.0.4" = {
        name = "postcss-reduce-transforms";
        packageName = "postcss-reduce-transforms";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-reduce-transforms/-/postcss-reduce-transforms-1.0.4.tgz";
          sha1 = "ff76f4d8212437b31c298a42d2e1444025771ae1";
        };
      };
      "postcss-replace-overflow-wrap-2.0.0" = {
        name = "postcss-replace-overflow-wrap";
        packageName = "postcss-replace-overflow-wrap";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-replace-overflow-wrap/-/postcss-replace-overflow-wrap-2.0.0.tgz";
          sha1 = "794db6faa54f8db100854392a93af45768b4e25b";
        };
      };
      "postcss-selector-matches-3.0.1" = {
        name = "postcss-selector-matches";
        packageName = "postcss-selector-matches";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-selector-matches/-/postcss-selector-matches-3.0.1.tgz";
          sha1 = "e5634011e13950881861bbdd58c2d0111ffc96ab";
        };
      };
      "postcss-selector-not-3.0.1" = {
        name = "postcss-selector-not";
        packageName = "postcss-selector-not";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-selector-not/-/postcss-selector-not-3.0.1.tgz";
          sha1 = "2e4db2f0965336c01e7cec7db6c60dff767335d9";
        };
      };
      "postcss-selector-parser-2.2.3" = {
        name = "postcss-selector-parser";
        packageName = "postcss-selector-parser";
        version = "2.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-selector-parser/-/postcss-selector-parser-2.2.3.tgz";
          sha1 = "f9437788606c3c9acee16ffe8d8b16297f27bb90";
        };
      };
      "postcss-svgo-2.1.6" = {
        name = "postcss-svgo";
        packageName = "postcss-svgo";
        version = "2.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-svgo/-/postcss-svgo-2.1.6.tgz";
          sha1 = "b6df18aa613b666e133f08adb5219c2684ac108d";
        };
      };
      "postcss-unique-selectors-2.0.2" = {
        name = "postcss-unique-selectors";
        packageName = "postcss-unique-selectors";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-unique-selectors/-/postcss-unique-selectors-2.0.2.tgz";
          sha1 = "981d57d29ddcb33e7b1dfe1fd43b8649f933ca1d";
        };
      };
      "postcss-value-parser-3.3.0" = {
        name = "postcss-value-parser";
        packageName = "postcss-value-parser";
        version = "3.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-value-parser/-/postcss-value-parser-3.3.0.tgz";
          sha1 = "87f38f9f18f774a4ab4c8a232f5c5ce8872a9d15";
        };
      };
      "postcss-zindex-2.2.0" = {
        name = "postcss-zindex";
        packageName = "postcss-zindex";
        version = "2.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/postcss-zindex/-/postcss-zindex-2.2.0.tgz";
          sha1 = "d2109ddc055b91af67fc4cb3b025946639d2af22";
        };
      };
      "prelude-ls-1.1.2" = {
        name = "prelude-ls";
        packageName = "prelude-ls";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
          sha1 = "21932a549f5e52ffd9a827f570e04be62a97da54";
        };
      };
      "prepend-http-1.0.4" = {
        name = "prepend-http";
        packageName = "prepend-http";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/prepend-http/-/prepend-http-1.0.4.tgz";
          sha1 = "d4f4562b0ce3696e41ac52d0e002e57a635dc6dc";
        };
      };
      "preserve-0.2.0" = {
        name = "preserve";
        packageName = "preserve";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/preserve/-/preserve-0.2.0.tgz";
          sha1 = "815ed1f6ebc65926f865b310c0713bcb3315ce4b";
        };
      };
      "pretty-error-2.1.1" = {
        name = "pretty-error";
        packageName = "pretty-error";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/pretty-error/-/pretty-error-2.1.1.tgz";
          sha1 = "5f4f87c8f91e5ae3f3ba87ab4cf5e03b1a17f1a3";
        };
      };
      "pretty-ms-0.2.2" = {
        name = "pretty-ms";
        packageName = "pretty-ms";
        version = "0.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/pretty-ms/-/pretty-ms-0.2.2.tgz";
          sha1 = "da879a682ff33a37011046f13d627f67c73b84f6";
        };
      };
      "pretty-ms-2.1.0" = {
        name = "pretty-ms";
        packageName = "pretty-ms";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pretty-ms/-/pretty-ms-2.1.0.tgz";
          sha1 = "4257c256df3fb0b451d6affaab021884126981dc";
        };
      };
      "pretty-ms-3.1.0" = {
        name = "pretty-ms";
        packageName = "pretty-ms";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pretty-ms/-/pretty-ms-3.1.0.tgz";
          sha1 = "e9cac9c76bf6ee52fe942dd9c6c4213153b12881";
        };
      };
      "private-0.1.8" = {
        name = "private";
        packageName = "private";
        version = "0.1.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/private/-/private-0.1.8.tgz";
          sha512 = "2dgznnpxsgy9bgp4kfby1is72blvca4lhmqb3nlja8yiig1v52c12p5yw0aag8jqazhkqvihpxmqf9gsjlg5dr1jb56jxzgnqrazy2n";
        };
      };
      "process-0.11.10" = {
        name = "process";
        packageName = "process";
        version = "0.11.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/process/-/process-0.11.10.tgz";
          sha1 = "7332300e840161bda3e69a1d1d91a7d4bc16f182";
        };
      };
      "process-nextick-args-2.0.0" = {
        name = "process-nextick-args";
        packageName = "process-nextick-args";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.0.tgz";
          sha512 = "0rw8xpqqkhs91722slvzf8icxfaimqp4w8zb3840jxr7r8n8035byl6dhdi5bm0yr6x7sdws0gf3m025fg6hqgaklwlbl4d7bah5l9j";
        };
      };
      "promise-7.3.1" = {
        name = "promise";
        packageName = "promise";
        version = "7.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/promise/-/promise-7.3.1.tgz";
          sha512 = "17cn4nns2nxh9r0pdiqsqx3fpvaa82c1mhcr8r84k2a9hkpb0mj4bxzfbg3l9iy74yn9hj6mh2gsddsi3v939a1zp7ycbzqkxfm12cy";
        };
      };
      "promise-inflight-1.0.1" = {
        name = "promise-inflight";
        packageName = "promise-inflight";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/promise-inflight/-/promise-inflight-1.0.1.tgz";
          sha1 = "98472870bf228132fcbdd868129bad12c3c029e3";
        };
      };
      "promise-retry-1.1.1" = {
        name = "promise-retry";
        packageName = "promise-retry";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/promise-retry/-/promise-retry-1.1.1.tgz";
          sha1 = "6739e968e3051da20ce6497fb2b50f6911df3d6d";
        };
      };
      "prop-types-15.6.1" = {
        name = "prop-types";
        packageName = "prop-types";
        version = "15.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/prop-types/-/prop-types-15.6.1.tgz";
          sha512 = "28vp2j7wpa97ygi4clflilfs8wj8qsz1x3fismsqx293a9cvpyc20k4pbrppyn7y6hks0dp3mn0wmj5q3y1y3i5k7ra5ssqimnkprz1";
        };
      };
      "proxy-addr-2.0.3" = {
        name = "proxy-addr";
        packageName = "proxy-addr";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/proxy-addr/-/proxy-addr-2.0.3.tgz";
          sha512 = "1fjf093wrxz3nm9njw491r9rvqwrcv8d8h0mnmyakpijnbff7lllysr7aq9h9w7jdvxjvaf9r055ywd7sghcqxwa2nfpdc9423c414d";
        };
      };
      "prr-1.0.1" = {
        name = "prr";
        packageName = "prr";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/prr/-/prr-1.0.1.tgz";
          sha1 = "d3fc114ba06995a45ec6893f484ceb1d78f5f476";
        };
      };
      "pseudomap-1.0.2" = {
        name = "pseudomap";
        packageName = "pseudomap";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/pseudomap/-/pseudomap-1.0.2.tgz";
          sha1 = "f052a28da70e618917ef0a8ac34c1ae5a68286b3";
        };
      };
      "public-encrypt-4.0.0" = {
        name = "public-encrypt";
        packageName = "public-encrypt";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/public-encrypt/-/public-encrypt-4.0.0.tgz";
          sha1 = "39f699f3a46560dd5ebacbca693caf7c65c18cc6";
        };
      };
      "pump-2.0.1" = {
        name = "pump";
        packageName = "pump";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/pump/-/pump-2.0.1.tgz";
          sha512 = "288hcmlwdnqda84ylx9cv413ic0r59k0dp71hy7a200jsb7h1y63277jwdp1jdp13c1b3pl6g2gzr5gjv9p72f5sp7w3p0d34swrqxf";
        };
      };
      "pumpify-1.4.0" = {
        name = "pumpify";
        packageName = "pumpify";
        version = "1.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/pumpify/-/pumpify-1.4.0.tgz";
          sha512 = "1h37biy199n445y10vpyiswwcxv8zigfqp0b1xwgbyjq51f2dhjn1pcggjc4j5ccbd64l1ivfi0bqinx4m5clcawvwggy7jv93qsjfs";
        };
      };
      "punycode-1.3.2" = {
        name = "punycode";
        packageName = "punycode";
        version = "1.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/punycode/-/punycode-1.3.2.tgz";
          sha1 = "9653a036fb7c1ee42342f2325cceefea3926c48d";
        };
      };
      "punycode-1.4.1" = {
        name = "punycode";
        packageName = "punycode";
        version = "1.4.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/punycode/-/punycode-1.4.1.tgz";
          sha1 = "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e";
        };
      };
      "punycode-2.1.0" = {
        name = "punycode";
        packageName = "punycode";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/punycode/-/punycode-2.1.0.tgz";
          sha1 = "5f863edc89b96db09074bad7947bf09056ca4e7d";
        };
      };
      "purescript-psa-0.5.1" = {
        name = "purescript-psa";
        packageName = "purescript-psa";
        version = "0.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/purescript-psa/-/purescript-psa-0.5.1.tgz";
          sha1 = "2e435eef22bd2f57eba182cbff120022ca0f265f";
        };
      };
      "purs-loader-3.1.2" = {
        name = "purs-loader";
        packageName = "purs-loader";
        version = "3.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/purs-loader/-/purs-loader-3.1.2.tgz";
          sha512 = "3l7h8qnsjgvcnswiw27y3pnjfzhj85xarmxk8k0dgyfwnfz7zahhnqmmgsyls8lq9nz66zf9swdjgjx8cf3h2z2l4cvy9a42gx00x8s";
        };
      };
      "q-1.5.1" = {
        name = "q";
        packageName = "q";
        version = "1.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/q/-/q-1.5.1.tgz";
          sha1 = "7e32f75b41381291d04611f1bf14109ac00651d7";
        };
      };
      "qs-6.5.1" = {
        name = "qs";
        packageName = "qs";
        version = "6.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/qs/-/qs-6.5.1.tgz";
          sha512 = "3waqapyj1k4g135sgj636rmswiaixq19is1rw0rpv4qp6k7dl0a9nwy06m7yl5lbdk9p6xpwwngnggbzlzaz6rh11c86j2nvnnf273r";
        };
      };
      "query-string-4.3.4" = {
        name = "query-string";
        packageName = "query-string";
        version = "4.3.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/query-string/-/query-string-4.3.4.tgz";
          sha1 = "bbb693b9ca915c232515b228b1a02b609043dbeb";
        };
      };
      "querystring-0.2.0" = {
        name = "querystring";
        packageName = "querystring";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/querystring/-/querystring-0.2.0.tgz";
          sha1 = "b209849203bb25df820da756e747005878521620";
        };
      };
      "querystring-es3-0.2.1" = {
        name = "querystring-es3";
        packageName = "querystring-es3";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/querystring-es3/-/querystring-es3-0.2.1.tgz";
          sha1 = "9ec61f79049875707d69414596fd907a4d711e73";
        };
      };
      "querystringify-0.0.4" = {
        name = "querystringify";
        packageName = "querystringify";
        version = "0.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/querystringify/-/querystringify-0.0.4.tgz";
          sha1 = "0cf7f84f9463ff0ae51c4c4b142d95be37724d9c";
        };
      };
      "querystringify-1.0.0" = {
        name = "querystringify";
        packageName = "querystringify";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/querystringify/-/querystringify-1.0.0.tgz";
          sha1 = "6286242112c5b712fa654e526652bf6a13ff05cb";
        };
      };
      "randomatic-1.1.7" = {
        name = "randomatic";
        packageName = "randomatic";
        version = "1.1.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/randomatic/-/randomatic-1.1.7.tgz";
          sha512 = "2is2kipfnz3hl4yxgqk07rll6956cq3zzf9cddai3f0lij5acq76v98qv14qkpljh1pqfsyb8p69xa9cyaww6p0j91s4vc9zj6594hg";
        };
      };
      "randombytes-2.0.6" = {
        name = "randombytes";
        packageName = "randombytes";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/randombytes/-/randombytes-2.0.6.tgz";
          sha512 = "3a0zyz736klfzzpd9vwag3gznq7lrj57igm474dq279gsnyqdgfm1brhrs78ky30gsdwz9rwnjjms00fpdpp2p3x8p9mq2zbhw3k108";
        };
      };
      "randomfill-1.0.4" = {
        name = "randomfill";
        packageName = "randomfill";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/randomfill/-/randomfill-1.0.4.tgz";
          sha512 = "0pm7c7mw7a3qwjr21f8cvxaa2sq5l4svqs51lppn833x0yvz3yx8x4vbd4rswjynykvlgvn4hrpq327pvbzp428f4b1fciy3xnmrfgk";
        };
      };
      "range-parser-1.2.0" = {
        name = "range-parser";
        packageName = "range-parser";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/range-parser/-/range-parser-1.2.0.tgz";
          sha1 = "f49be6b487894ddc40dcc94a322f611092e00d5e";
        };
      };
      "raw-body-2.3.2" = {
        name = "raw-body";
        packageName = "raw-body";
        version = "2.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/raw-body/-/raw-body-2.3.2.tgz";
          sha1 = "bcd60c77d3eb93cde0050295c3f379389bc88f89";
        };
      };
      "rc-1.2.5" = {
        name = "rc";
        packageName = "rc";
        version = "1.2.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/rc/-/rc-1.2.5.tgz";
          sha1 = "275cd687f6e3b36cc756baa26dfee80a790301fd";
        };
      };
      "react-16.2.0" = {
        name = "react";
        packageName = "react";
        version = "16.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/react/-/react-16.2.0.tgz";
          sha512 = "06jpx4ipg4fqmn52gz95i8hn5m5hvhz48pnqkq9c045xzwyabsdd5p9qij10x79xm3hzr356bvnrmczv40i9lj97kpm04y4rsc2hqk6";
        };
      };
      "react-dom-16.2.0" = {
        name = "react-dom";
        packageName = "react-dom";
        version = "16.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/react-dom/-/react-dom-16.2.0.tgz";
          sha512 = "27fzqr9bpraww7c66rib1znjqyj6jrpsrhw94zqmbgjlgfbkrylyq5x1anm7bmx2h0w5qbwha7anjj7zqdglpfxsfsd57ym05vq14ff";
        };
      };
      "read-cache-1.0.0" = {
        name = "read-cache";
        packageName = "read-cache";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/read-cache/-/read-cache-1.0.0.tgz";
          sha1 = "e664ef31161166c9751cdbe8dbcf86b5fb58f774";
        };
      };
      "read-pkg-1.1.0" = {
        name = "read-pkg";
        packageName = "read-pkg";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/read-pkg/-/read-pkg-1.1.0.tgz";
          sha1 = "f5ffaa5ecd29cb31c0474bca7d756b6bb29e3f28";
        };
      };
      "read-pkg-2.0.0" = {
        name = "read-pkg";
        packageName = "read-pkg";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/read-pkg/-/read-pkg-2.0.0.tgz";
          sha1 = "8ef1c0623c6a6db0dc6713c4bfac46332b2368f8";
        };
      };
      "read-pkg-up-1.0.1" = {
        name = "read-pkg-up";
        packageName = "read-pkg-up";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/read-pkg-up/-/read-pkg-up-1.0.1.tgz";
          sha1 = "9d63c13276c065918d57f002a57f40a1b643fb02";
        };
      };
      "read-pkg-up-2.0.0" = {
        name = "read-pkg-up";
        packageName = "read-pkg-up";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/read-pkg-up/-/read-pkg-up-2.0.0.tgz";
          sha1 = "6b72a8048984e0c41e79510fd5e9fa99b3b549be";
        };
      };
      "readable-stream-1.0.34" = {
        name = "readable-stream";
        packageName = "readable-stream";
        version = "1.0.34";
        src = fetchurl {
          url = "https://registry.npmjs.org/readable-stream/-/readable-stream-1.0.34.tgz";
          sha1 = "125820e34bc842d2f2aaafafe4c2916ee32c157c";
        };
      };
      "readable-stream-2.3.5" = {
        name = "readable-stream";
        packageName = "readable-stream";
        version = "2.3.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.5.tgz";
          sha512 = "09n3j4jsfl2lq3gj65qwn5b3lvzb624lrb8m14h81ls9cw59vvm8436gm5zwaqxf6y1zrfwbrsmizsq48jw1s3qj9zhpjp438735bdl";
        };
      };
      "readdirp-2.1.0" = {
        name = "readdirp";
        packageName = "readdirp";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/readdirp/-/readdirp-2.1.0.tgz";
          sha1 = "4ed0ad060df3073300c48440373f72d1cc642d78";
        };
      };
      "redent-1.0.0" = {
        name = "redent";
        packageName = "redent";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/redent/-/redent-1.0.0.tgz";
          sha1 = "cf916ab1fd5f1f16dfb20822dd6ec7f730c2afde";
        };
      };
      "reduce-css-calc-1.3.0" = {
        name = "reduce-css-calc";
        packageName = "reduce-css-calc";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/reduce-css-calc/-/reduce-css-calc-1.3.0.tgz";
          sha1 = "747c914e049614a4c9cfbba629871ad1d2927716";
        };
      };
      "reduce-css-calc-2.1.4" = {
        name = "reduce-css-calc";
        packageName = "reduce-css-calc";
        version = "2.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/reduce-css-calc/-/reduce-css-calc-2.1.4.tgz";
          sha512 = "0av6q70s10wxq8m6330sw0jvbyp05xzkaxgl0ql1khgk5q55d3mqsqxdwmd0nywf8zhiznz68iafppmima3jz9ak98s5pcxpi0xdywb";
        };
      };
      "reduce-function-call-1.0.2" = {
        name = "reduce-function-call";
        packageName = "reduce-function-call";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/reduce-function-call/-/reduce-function-call-1.0.2.tgz";
          sha1 = "5a200bf92e0e37751752fe45b0ab330fd4b6be99";
        };
      };
      "regenerate-1.3.3" = {
        name = "regenerate";
        packageName = "regenerate";
        version = "1.3.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/regenerate/-/regenerate-1.3.3.tgz";
          sha512 = "2rbbgk0fggg02f798h8whyk1d5ky4pqmbjdaia7hzfpmxd16dy4nmd0wd6qr1xgic5434h305cmhp75f4sd4zyvww531b4xcva6hnld";
        };
      };
      "regenerator-runtime-0.10.5" = {
        name = "regenerator-runtime";
        packageName = "regenerator-runtime";
        version = "0.10.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.10.5.tgz";
          sha1 = "336c3efc1220adcedda2c9fab67b5a7955a33658";
        };
      };
      "regenerator-runtime-0.11.1" = {
        name = "regenerator-runtime";
        packageName = "regenerator-runtime";
        version = "0.11.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.11.1.tgz";
          sha512 = "03d4l8l8cyywh93wf5vw84lq56jh1b1d7jll4ny4z060j9hvx7w5q3q0b8q227jm93749k1c9h86r2pz0bm2xq5vp14g3r2kbvqc2rj";
        };
      };
      "regenerator-transform-0.10.1" = {
        name = "regenerator-transform";
        packageName = "regenerator-transform";
        version = "0.10.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/regenerator-transform/-/regenerator-transform-0.10.1.tgz";
          sha512 = "3lxj64s9v2jksipiz0pphfy6m12cf7j96w64bqlzjwnasbpqhc5ws5bjzv3x9ld625v1z4f15l30izpv3p6l9w8h1qsvf6vy1pak5rw";
        };
      };
      "regex-cache-0.4.4" = {
        name = "regex-cache";
        packageName = "regex-cache";
        version = "0.4.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/regex-cache/-/regex-cache-0.4.4.tgz";
          sha512 = "1crfmf19zkv0imnbbkj7bwrcyin3zxa88cs86b6apkxj8qrsmkxnydhsy2ia75q4ld10rhi2s2c36h7g77a997mh9c2z453s311jllx";
        };
      };
      "regex-not-1.0.2" = {
        name = "regex-not";
        packageName = "regex-not";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/regex-not/-/regex-not-1.0.2.tgz";
          sha512 = "3cggngaj8m70zdn8kghha4mhvavm7jfy5xm2iqi94w4gi5m5irs3nlrgg975w2231y49jnnw7zhsg648pbkl9zb6vwhii83926q7917";
        };
      };
      "regexpu-core-1.0.0" = {
        name = "regexpu-core";
        packageName = "regexpu-core";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/regexpu-core/-/regexpu-core-1.0.0.tgz";
          sha1 = "86a763f58ee4d7c2f6b102e4764050de7ed90c6b";
        };
      };
      "regexpu-core-2.0.0" = {
        name = "regexpu-core";
        packageName = "regexpu-core";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/regexpu-core/-/regexpu-core-2.0.0.tgz";
          sha1 = "49d038837b8dcf8bfa5b9a42139938e6ea2ae240";
        };
      };
      "registry-auth-token-3.3.2" = {
        name = "registry-auth-token";
        packageName = "registry-auth-token";
        version = "3.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/registry-auth-token/-/registry-auth-token-3.3.2.tgz";
          sha512 = "2fv3fbyf4wv81famfkd3j8nld0fc4sfjfxfc91wwpqwgp00nl3727c9z4sdm4gicb4nxzmhkrphsnphpamgmknsh2ak15qpmmrzvg94";
        };
      };
      "registry-url-3.1.0" = {
        name = "registry-url";
        packageName = "registry-url";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/registry-url/-/registry-url-3.1.0.tgz";
          sha1 = "3d4ef870f73dde1d77f0cf9a381432444e174942";
        };
      };
      "regjsgen-0.2.0" = {
        name = "regjsgen";
        packageName = "regjsgen";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/regjsgen/-/regjsgen-0.2.0.tgz";
          sha1 = "6c016adeac554f75823fe37ac05b92d5a4edb1f7";
        };
      };
      "regjsparser-0.1.5" = {
        name = "regjsparser";
        packageName = "regjsparser";
        version = "0.1.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/regjsparser/-/regjsparser-0.1.5.tgz";
          sha1 = "7ee8f84dc6fa792d3fd0ae228d24bd949ead205c";
        };
      };
      "relateurl-0.2.7" = {
        name = "relateurl";
        packageName = "relateurl";
        version = "0.2.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/relateurl/-/relateurl-0.2.7.tgz";
          sha1 = "54dbf377e51440aca90a4cd274600d3ff2d888a9";
        };
      };
      "release-zalgo-1.0.0" = {
        name = "release-zalgo";
        packageName = "release-zalgo";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/release-zalgo/-/release-zalgo-1.0.0.tgz";
          sha1 = "09700b7e5074329739330e535c5a90fb67851730";
        };
      };
      "remove-trailing-separator-1.1.0" = {
        name = "remove-trailing-separator";
        packageName = "remove-trailing-separator";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/remove-trailing-separator/-/remove-trailing-separator-1.1.0.tgz";
          sha1 = "c24bce2a283adad5bc3f58e0d48249b92379d8ef";
        };
      };
      "renderkid-2.0.1" = {
        name = "renderkid";
        packageName = "renderkid";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/renderkid/-/renderkid-2.0.1.tgz";
          sha1 = "898cabfc8bede4b7b91135a3ffd323e58c0db319";
        };
      };
      "repeat-element-1.1.2" = {
        name = "repeat-element";
        packageName = "repeat-element";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/repeat-element/-/repeat-element-1.1.2.tgz";
          sha1 = "ef089a178d1483baae4d93eb98b4f9e4e11d990a";
        };
      };
      "repeat-string-1.6.1" = {
        name = "repeat-string";
        packageName = "repeat-string";
        version = "1.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/repeat-string/-/repeat-string-1.6.1.tgz";
          sha1 = "8dcae470e1c88abc2d600fff4a776286da75e637";
        };
      };
      "repeating-2.0.1" = {
        name = "repeating";
        packageName = "repeating";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/repeating/-/repeating-2.0.1.tgz";
          sha1 = "5214c53a926d3552707527fbab415dbc08d06dda";
        };
      };
      "replace-in-file-3.2.0" = {
        name = "replace-in-file";
        packageName = "replace-in-file";
        version = "3.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/replace-in-file/-/replace-in-file-3.2.0.tgz";
          sha512 = "2s50ciisbjs3079y56by2snnmbgngyh4wa2llq075382g5h4p0wbakcrvvnr4jgxm6z9vdls7d3cmd6f3zy3vwb6q66i147f5ha0dpn";
        };
      };
      "request-2.83.0" = {
        name = "request";
        packageName = "request";
        version = "2.83.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/request/-/request-2.83.0.tgz";
          sha512 = "0by1djkn836sqd9pk2c777wcjvp34qbk1plx7s4lmykljrblpjc64dvn6ni2vyxsbyk33wnl6avym8vgw0ggr4226xakck8mw7y07cm";
        };
      };
      "request-promise-core-1.1.1" = {
        name = "request-promise-core";
        packageName = "request-promise-core";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/request-promise-core/-/request-promise-core-1.1.1.tgz";
          sha1 = "3eee00b2c5aa83239cfb04c5700da36f81cd08b6";
        };
      };
      "request-promise-native-1.0.5" = {
        name = "request-promise-native";
        packageName = "request-promise-native";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/request-promise-native/-/request-promise-native-1.0.5.tgz";
          sha1 = "5281770f68e0c9719e5163fd3fab482215f4fda5";
        };
      };
      "require-directory-2.1.1" = {
        name = "require-directory";
        packageName = "require-directory";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
          sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
        };
      };
      "require-from-string-1.2.1" = {
        name = "require-from-string";
        packageName = "require-from-string";
        version = "1.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/require-from-string/-/require-from-string-1.2.1.tgz";
          sha1 = "529c9ccef27380adfec9a2f965b649bbee636418";
        };
      };
      "require-main-filename-1.0.1" = {
        name = "require-main-filename";
        packageName = "require-main-filename";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/require-main-filename/-/require-main-filename-1.0.1.tgz";
          sha1 = "97f717b69d48784f5f526a6c5aa8ffdda055a4d1";
        };
      };
      "require-precompiled-0.1.0" = {
        name = "require-precompiled";
        packageName = "require-precompiled";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/require-precompiled/-/require-precompiled-0.1.0.tgz";
          sha1 = "5a1b52eb70ebed43eb982e974c85ab59571e56fa";
        };
      };
      "requires-port-1.0.0" = {
        name = "requires-port";
        packageName = "requires-port";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/requires-port/-/requires-port-1.0.0.tgz";
          sha1 = "925d2601d39ac485e091cf0da5c6e694dc3dcaff";
        };
      };
      "resolve-1.5.0" = {
        name = "resolve";
        packageName = "resolve";
        version = "1.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve/-/resolve-1.5.0.tgz";
          sha512 = "25scf9zkhf5yc9x3d7mfq2im5vyxmq3ih939na6jzblal7mgfcijmadl2maz501mkccykj714gvdhhmlzi86hbk7k03r9ipnwd142l6";
        };
      };
      "resolve-cwd-1.0.0" = {
        name = "resolve-cwd";
        packageName = "resolve-cwd";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve-cwd/-/resolve-cwd-1.0.0.tgz";
          sha1 = "4eaeea41ed040d1702457df64a42b2b07d246f9f";
        };
      };
      "resolve-cwd-2.0.0" = {
        name = "resolve-cwd";
        packageName = "resolve-cwd";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve-cwd/-/resolve-cwd-2.0.0.tgz";
          sha1 = "00a9f7387556e27038eae232caa372a6a59b665a";
        };
      };
      "resolve-from-2.0.0" = {
        name = "resolve-from";
        packageName = "resolve-from";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve-from/-/resolve-from-2.0.0.tgz";
          sha1 = "9480ab20e94ffa1d9e80a804c7ea147611966b57";
        };
      };
      "resolve-from-3.0.0" = {
        name = "resolve-from";
        packageName = "resolve-from";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve-from/-/resolve-from-3.0.0.tgz";
          sha1 = "b22c7af7d9d6881bc8b6e653335eebcb0a188748";
        };
      };
      "resolve-url-0.2.1" = {
        name = "resolve-url";
        packageName = "resolve-url";
        version = "0.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/resolve-url/-/resolve-url-0.2.1.tgz";
          sha1 = "2c637fe77c893afd2a663fe21aa9080068e2052a";
        };
      };
      "restore-cursor-2.0.0" = {
        name = "restore-cursor";
        packageName = "restore-cursor";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/restore-cursor/-/restore-cursor-2.0.0.tgz";
          sha1 = "9f7ee287f82fd326d4fd162923d62129eee0dfaf";
        };
      };
      "ret-0.1.15" = {
        name = "ret";
        packageName = "ret";
        version = "0.1.15";
        src = fetchurl {
          url = "https://registry.npmjs.org/ret/-/ret-0.1.15.tgz";
          sha512 = "2d7igpgyzdlpx2ni0sql8gsnqk9qivfsw6bn1aklm19kbhgxjzmlazz8szfsbdpjka4gk6i3zf0jqa0llaf7dni636fnbwfmyjmhfad";
        };
      };
      "retry-0.10.1" = {
        name = "retry";
        packageName = "retry";
        version = "0.10.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/retry/-/retry-0.10.1.tgz";
          sha1 = "e76388d217992c252750241d3d3956fed98d8ff4";
        };
      };
      "rgb-0.1.0" = {
        name = "rgb";
        packageName = "rgb";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/rgb/-/rgb-0.1.0.tgz";
          sha1 = "be27b291e8feffeac1bd99729721bfa40fc037b5";
        };
      };
      "rgb-hex-2.1.0" = {
        name = "rgb-hex";
        packageName = "rgb-hex";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/rgb-hex/-/rgb-hex-2.1.0.tgz";
          sha1 = "c773c5fe2268a25578d92539a82a7a5ce53beda6";
        };
      };
      "right-align-0.1.3" = {
        name = "right-align";
        packageName = "right-align";
        version = "0.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/right-align/-/right-align-0.1.3.tgz";
          sha1 = "61339b722fe6a3515689210d24e14c96148613ef";
        };
      };
      "rimraf-2.6.2" = {
        name = "rimraf";
        packageName = "rimraf";
        version = "2.6.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/rimraf/-/rimraf-2.6.2.tgz";
          sha512 = "3kmrqh8xli7rzfm8wc6j9lp0c6vml172iv3z088an9xlwl1xvkvh3fn92za66ms4c9yww80qa5kan31k1z1ypqvkchmh1mznb09xdwn";
        };
      };
      "ripemd160-2.0.1" = {
        name = "ripemd160";
        packageName = "ripemd160";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ripemd160/-/ripemd160-2.0.1.tgz";
          sha1 = "0f4584295c53a3628af7e6d79aca21ce57d1c6e7";
        };
      };
      "run-queue-1.0.3" = {
        name = "run-queue";
        packageName = "run-queue";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/run-queue/-/run-queue-1.0.3.tgz";
          sha1 = "e848396f057d223f24386924618e25694161ec47";
        };
      };
      "safe-buffer-5.1.1" = {
        name = "safe-buffer";
        packageName = "safe-buffer";
        version = "5.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.1.tgz";
          sha512 = "1p28rllll1w65yzq5azi4izx962399xdsdlfbaynn7vmp981hiss05jhiy9hm7sbbfk3b4dhlcv0zy07fc59mnc07hdv6wcgqkcvawh";
        };
      };
      "safe-regex-1.1.0" = {
        name = "safe-regex";
        packageName = "safe-regex";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/safe-regex/-/safe-regex-1.1.0.tgz";
          sha1 = "40a3669f3b077d1e943d44629e157dd48023bf2e";
        };
      };
      "sax-1.2.4" = {
        name = "sax";
        packageName = "sax";
        version = "1.2.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/sax/-/sax-1.2.4.tgz";
          sha512 = "1dn291mjsda42w8kldlbmngk6dhjxfbvvd5lckyqmwbjaj6069iq3wx0nvcfglwnpddz2qa93lzf4hv77iz43bd2qixa079sjzl799n";
        };
      };
      "schema-utils-0.3.0" = {
        name = "schema-utils";
        packageName = "schema-utils";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/schema-utils/-/schema-utils-0.3.0.tgz";
          sha1 = "f5877222ce3e931edae039f17eb3716e7137f8cf";
        };
      };
      "schema-utils-0.4.5" = {
        name = "schema-utils";
        packageName = "schema-utils";
        version = "0.4.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/schema-utils/-/schema-utils-0.4.5.tgz";
          sha512 = "1y97jl7qm7xvd867cghrgfzrlyf5z05rvwpigvi4ijzy4c8ckqr0bl76ym5nh6akiyc2qb19kbnmmk1r8nkfm4fsp4lznnpsipy72n9";
        };
      };
      "select-hose-2.0.0" = {
        name = "select-hose";
        packageName = "select-hose";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/select-hose/-/select-hose-2.0.0.tgz";
          sha1 = "625d8658f865af43ec962bfc376a37359a4994ca";
        };
      };
      "selfsigned-1.10.2" = {
        name = "selfsigned";
        packageName = "selfsigned";
        version = "1.10.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/selfsigned/-/selfsigned-1.10.2.tgz";
          sha1 = "b4449580d99929b65b10a48389301a6592088758";
        };
      };
      "semver-5.5.0" = {
        name = "semver";
        packageName = "semver";
        version = "5.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/semver/-/semver-5.5.0.tgz";
          sha512 = "0h32zh035y8m6dzcqhcymbhwgmc8839fa1hhj0jfh9ivp9kmqfj1sbwnsnkzcn9qm3sqn38sa8ys2g4c638lpnmzjr0a0qndmv7f8p1";
        };
      };
      "semver-diff-2.1.0" = {
        name = "semver-diff";
        packageName = "semver-diff";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/semver-diff/-/semver-diff-2.1.0.tgz";
          sha1 = "4bbb8437c8d37e4b0cf1a68fd726ec6d645d6d36";
        };
      };
      "send-0.16.1" = {
        name = "send";
        packageName = "send";
        version = "0.16.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/send/-/send-0.16.1.tgz";
          sha512 = "3c9rfxzsayrnka50s3hdbln9sjzad94ll4z2nx83i3rqciy4dxj05x34sjmm64k46zmk99pj8g4bcwk476a3iqzpcxgja28s8jqnl0j";
        };
      };
      "serialize-javascript-1.4.0" = {
        name = "serialize-javascript";
        packageName = "serialize-javascript";
        version = "1.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/serialize-javascript/-/serialize-javascript-1.4.0.tgz";
          sha1 = "7c958514db6ac2443a8abc062dc9f7886a7f6005";
        };
      };
      "serve-index-1.9.1" = {
        name = "serve-index";
        packageName = "serve-index";
        version = "1.9.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/serve-index/-/serve-index-1.9.1.tgz";
          sha1 = "d3768d69b1e7d82e5ce050fff5b453bea12a9239";
        };
      };
      "serve-static-1.13.1" = {
        name = "serve-static";
        packageName = "serve-static";
        version = "1.13.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/serve-static/-/serve-static-1.13.1.tgz";
          sha512 = "2ahchxbzy0wr61gjy85p35cx4rkfb5347fmglk5rb2wawla3nhx6xx8hsgvmvjcsp5vfdilvf84kcnvp832f1anylsg4sqgpdk188w5";
        };
      };
      "set-blocking-2.0.0" = {
        name = "set-blocking";
        packageName = "set-blocking";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz";
          sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
        };
      };
      "set-getter-0.1.0" = {
        name = "set-getter";
        packageName = "set-getter";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/set-getter/-/set-getter-0.1.0.tgz";
          sha1 = "d769c182c9d5a51f409145f2fba82e5e86e80376";
        };
      };
      "set-immediate-shim-1.0.1" = {
        name = "set-immediate-shim";
        packageName = "set-immediate-shim";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/set-immediate-shim/-/set-immediate-shim-1.0.1.tgz";
          sha1 = "4b2b1b27eb808a9f8dcc481a58e5e56f599f3f61";
        };
      };
      "set-value-0.4.3" = {
        name = "set-value";
        packageName = "set-value";
        version = "0.4.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/set-value/-/set-value-0.4.3.tgz";
          sha1 = "7db08f9d3d22dc7f78e53af3c3bf4666ecdfccf1";
        };
      };
      "set-value-2.0.0" = {
        name = "set-value";
        packageName = "set-value";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/set-value/-/set-value-2.0.0.tgz";
          sha512 = "1xdxg14zh452ih8f7826ki7xpq8wk8a831pm5zngqf8cbc4qv6mr9npks863bfqylfrhm161whf9199rmqn4i12wzmz2ks69z3343c7";
        };
      };
      "setimmediate-1.0.5" = {
        name = "setimmediate";
        packageName = "setimmediate";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/setimmediate/-/setimmediate-1.0.5.tgz";
          sha1 = "290cbb232e306942d7d7ea9b83732ab7856f8285";
        };
      };
      "setprototypeof-1.0.3" = {
        name = "setprototypeof";
        packageName = "setprototypeof";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.0.3.tgz";
          sha1 = "66567e37043eeb4f04d91bd658c0cbefb55b8e04";
        };
      };
      "setprototypeof-1.1.0" = {
        name = "setprototypeof";
        packageName = "setprototypeof";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.1.0.tgz";
          sha512 = "2jlhhawfqdiga1m6if01ks1q3yx56k5vj6wf372589vkswvdflw7224viivxali56b0jjsckpmjy10rj6fcakhw2dbq2psr197kzw86";
        };
      };
      "sha.js-2.4.10" = {
        name = "sha.js";
        packageName = "sha.js";
        version = "2.4.10";
        src = fetchurl {
          url = "https://registry.npmjs.org/sha.js/-/sha.js-2.4.10.tgz";
          sha512 = "2lfna0mg4mzdki4p3q29rsgywbghvy6f6jy6b61zj68d2d936wfqjgqpsdjchfcqkiim53qknpcnq9iiafyidfrw154qf75a2n2cz5y";
        };
      };
      "shebang-command-1.2.0" = {
        name = "shebang-command";
        packageName = "shebang-command";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/shebang-command/-/shebang-command-1.2.0.tgz";
          sha1 = "44aac65b695b03398968c39f363fee5deafdf1ea";
        };
      };
      "shebang-regex-1.0.0" = {
        name = "shebang-regex";
        packageName = "shebang-regex";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-1.0.0.tgz";
          sha1 = "da42f49740c0b42db2ca9728571cb190c98efea3";
        };
      };
      "signal-exit-3.0.2" = {
        name = "signal-exit";
        packageName = "signal-exit";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.2.tgz";
          sha1 = "b5fdc08f1287ea1178628e415e25132b73646c6d";
        };
      };
      "simple-swizzle-0.2.2" = {
        name = "simple-swizzle";
        packageName = "simple-swizzle";
        version = "0.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/simple-swizzle/-/simple-swizzle-0.2.2.tgz";
          sha1 = "a4da6b635ffcccca33f70d17cb92592de95e557a";
        };
      };
      "slash-1.0.0" = {
        name = "slash";
        packageName = "slash";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/slash/-/slash-1.0.0.tgz";
          sha1 = "c41f2f6c39fc16d1cd17ad4b5d896114ae470d55";
        };
      };
      "slice-ansi-1.0.0" = {
        name = "slice-ansi";
        packageName = "slice-ansi";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/slice-ansi/-/slice-ansi-1.0.0.tgz";
          sha512 = "1xd3zsk02nck4y601rn98n8cicrphaw5bdix278mk1yizmjv9s0wpa6akcqggd7d99c55s3byf4ylqdxkshyfsfnfx7lvwbmq2b3siw";
        };
      };
      "slide-1.1.6" = {
        name = "slide";
        packageName = "slide";
        version = "1.1.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/slide/-/slide-1.1.6.tgz";
          sha1 = "56eb027d65b4d2dce6cb2e2d32c4d4afc9e1d707";
        };
      };
      "snapdragon-0.8.1" = {
        name = "snapdragon";
        packageName = "snapdragon";
        version = "0.8.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/snapdragon/-/snapdragon-0.8.1.tgz";
          sha1 = "e12b5487faded3e3dea0ac91e9400bf75b401370";
        };
      };
      "snapdragon-node-2.1.1" = {
        name = "snapdragon-node";
        packageName = "snapdragon-node";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/snapdragon-node/-/snapdragon-node-2.1.1.tgz";
          sha512 = "2gk18pdld8ij1bpa2mdwl8f7i4rl5d4ys3qw31hipj56wslnsfhp1vxp3q36kj1m4f34wzzlvj0282qx5xlflqf978xyqlc2viyaviv";
        };
      };
      "snapdragon-util-3.0.1" = {
        name = "snapdragon-util";
        packageName = "snapdragon-util";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/snapdragon-util/-/snapdragon-util-3.0.1.tgz";
          sha512 = "1jsaqma4ycl2iq0761i1w7758z1kq7gbsij4xfb7p5cnw0qa62pszv6pr3j856n3pbxww7wwxs5wvcg2cb6vy020kw3bchashqs9clr";
        };
      };
      "sntp-2.1.0" = {
        name = "sntp";
        packageName = "sntp";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/sntp/-/sntp-2.1.0.tgz";
          sha512 = "0k2smmr24w5hb1cpql6vcgh58vzp4pmh9anf0bgz3arlsgq1mapnlq9fjqr6xs10aq1cmxaw987fwknqi62frax0fvs9bj3q3kmpg8l";
        };
      };
      "socket.io-client-2.0.4" = {
        name = "socket.io-client";
        packageName = "socket.io-client";
        version = "2.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/socket.io-client/-/socket.io-client-2.0.4.tgz";
          sha1 = "0918a552406dc5e540b380dcd97afc4a64332f8e";
        };
      };
      "socket.io-parser-3.1.3" = {
        name = "socket.io-parser";
        packageName = "socket.io-parser";
        version = "3.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/socket.io-parser/-/socket.io-parser-3.1.3.tgz";
          sha512 = "3pd3lbxawkd1zs739v0vv81vhf6pyax989j96hrc7vhql0xv7kn13sr893hrnn6m71jngw5h61523dq0b0p8drvff0fm0lbz8fbcil3";
        };
      };
      "sockjs-0.3.19" = {
        name = "sockjs";
        packageName = "sockjs";
        version = "0.3.19";
        src = fetchurl {
          url = "https://registry.npmjs.org/sockjs/-/sockjs-0.3.19.tgz";
          sha512 = "0pn77r3rgi2blxla6ilhf4madx8n0cprzb295mw5knx2hyhiwn60z8n8n8clsb1l0wddcrjj5x4rn24ydf2wnxbrn63xwb5lsa293sp";
        };
      };
      "sockjs-client-1.1.4" = {
        name = "sockjs-client";
        packageName = "sockjs-client";
        version = "1.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/sockjs-client/-/sockjs-client-1.1.4.tgz";
          sha1 = "5babe386b775e4cf14e7520911452654016c8b12";
        };
      };
      "sort-keys-1.1.2" = {
        name = "sort-keys";
        packageName = "sort-keys";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/sort-keys/-/sort-keys-1.1.2.tgz";
          sha1 = "441b6d4d346798f1b4e49e8920adfba0e543f9ad";
        };
      };
      "sort-keys-2.0.0" = {
        name = "sort-keys";
        packageName = "sort-keys";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/sort-keys/-/sort-keys-2.0.0.tgz";
          sha1 = "658535584861ec97d730d6cf41822e1f56684128";
        };
      };
      "source-list-map-2.0.0" = {
        name = "source-list-map";
        packageName = "source-list-map";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-list-map/-/source-list-map-2.0.0.tgz";
          sha512 = "3q09f2w67qqhl3lwiisj4422mj9nfldg4cxmidfrjcwn3k7spm9g46x4n1j6kv39bi9khmcpyvfa3fwski488ibivyg9bwijjw2cr93";
        };
      };
      "source-map-0.5.7" = {
        name = "source-map";
        packageName = "source-map";
        version = "0.5.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map/-/source-map-0.5.7.tgz";
          sha1 = "8a039d2d1021d22d1ea14c80d8ea468ba2ef3fcc";
        };
      };
      "source-map-0.6.1" = {
        name = "source-map";
        packageName = "source-map";
        version = "0.6.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz";
          sha512 = "3p7hw8p69ikj5mwapmqkacsjnbvdfk5ylyamjg9x5izkl717xvzj0vk3fnmx1n4pf54h5rs7r8ig5kk4jv4ycqqj0hv75cnx6k1lf2j";
        };
      };
      "source-map-loader-0.2.3" = {
        name = "source-map-loader";
        packageName = "source-map-loader";
        version = "0.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map-loader/-/source-map-loader-0.2.3.tgz";
          sha512 = "0dl4vmd8009szk6jq52afd27hf3v7p20v5z8dbggdmjz0pw7h7a3aq49ysny5mg90vihqya06kp0xnm8b0vzdpw2p869inqs1gwb1ii";
        };
      };
      "source-map-resolve-0.5.1" = {
        name = "source-map-resolve";
        packageName = "source-map-resolve";
        version = "0.5.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map-resolve/-/source-map-resolve-0.5.1.tgz";
          sha512 = "3ccyfzn4imm9m891wy0bqh85lxrsf82snlh7dlgvjc28rpd2m6n95x8kjmm2crcpqv6234xc2lqzp1h1cyx7xrn146nzinzzk1bd9fh";
        };
      };
      "source-map-support-0.4.18" = {
        name = "source-map-support";
        packageName = "source-map-support";
        version = "0.4.18";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map-support/-/source-map-support-0.4.18.tgz";
          sha512 = "1n37icn5xpsxs2x8szv6ifajjf066fskm04xn6agr79sid57n0yws4n0cis7m9q5hr0hxzr8dv2fnmmpgb4mvz8kiyv2g5ikbyb9g5n";
        };
      };
      "source-map-url-0.4.0" = {
        name = "source-map-url";
        packageName = "source-map-url";
        version = "0.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/source-map-url/-/source-map-url-0.4.0.tgz";
          sha1 = "3e935d7ddd73631b97659956d55128e87b5084a3";
        };
      };
      "spdx-correct-3.0.0" = {
        name = "spdx-correct";
        packageName = "spdx-correct";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdx-correct/-/spdx-correct-3.0.0.tgz";
          sha512 = "3xgkqk4hsm4nzvy68icv1rlv4s0qgb7mwpi9s4z2mp1a5fwny6kc6m4nqa7zzx2k7mrjlh7psci9lb7892bl3q4r4y2f4sw7vvnhprp";
        };
      };
      "spdx-exceptions-2.1.0" = {
        name = "spdx-exceptions";
        packageName = "spdx-exceptions";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdx-exceptions/-/spdx-exceptions-2.1.0.tgz";
          sha512 = "057m2ab3y8j38xb89w7d191hfaa693vbf7wnwkf302zicsgi2v5ayvcc4f03vyj4yyycqasqp3ryp2d3q0vcaq54r4ls2g5dar4vbg0";
        };
      };
      "spdx-expression-parse-3.0.0" = {
        name = "spdx-expression-parse";
        packageName = "spdx-expression-parse";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdx-expression-parse/-/spdx-expression-parse-3.0.0.tgz";
          sha512 = "351djgqvsgqmfg6h764c2k09dmixczw5073jirm8km6i1yym4xjrzc7g5ckwkidi3gls7s910m4ahl8sh37dsb478j8j3sigbfq63k2";
        };
      };
      "spdx-license-ids-3.0.0" = {
        name = "spdx-license-ids";
        packageName = "spdx-license-ids";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-3.0.0.tgz";
          sha512 = "066gb8vcffv2mf3lbmxvxp5b60jviw32x52n50l3zgl9z4cgwg74ca4kr4y5c3mss736bmg9nxl1lvprishbikqy0jy56770v10zqfv";
        };
      };
      "spdy-3.4.7" = {
        name = "spdy";
        packageName = "spdy";
        version = "3.4.7";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdy/-/spdy-3.4.7.tgz";
          sha1 = "42ff41ece5cc0f99a3a6c28aabb73f5c3b03acbc";
        };
      };
      "spdy-transport-2.0.20" = {
        name = "spdy-transport";
        packageName = "spdy-transport";
        version = "2.0.20";
        src = fetchurl {
          url = "https://registry.npmjs.org/spdy-transport/-/spdy-transport-2.0.20.tgz";
          sha1 = "735e72054c486b2354fe89e702256004a39ace4d";
        };
      };
      "split-string-3.1.0" = {
        name = "split-string";
        packageName = "split-string";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/split-string/-/split-string-3.1.0.tgz";
          sha512 = "25ih1dx2qb3lawqjxj85znd4l3x8nnigrcdlpfw8064gh2mwxic9bgg5ylgxm9gjl3v8dmyc47rycp8xvqz78jqalg0g9yqj225acrp";
        };
      };
      "sprintf-js-1.0.3" = {
        name = "sprintf-js";
        packageName = "sprintf-js";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
          sha1 = "04e6926f662895354f3dd015203633b857297e2c";
        };
      };
      "sshpk-1.13.1" = {
        name = "sshpk";
        packageName = "sshpk";
        version = "1.13.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/sshpk/-/sshpk-1.13.1.tgz";
          sha1 = "512df6da6287144316dc4c18fe1cf1d940739be3";
        };
      };
      "ssri-5.2.4" = {
        name = "ssri";
        packageName = "ssri";
        version = "5.2.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/ssri/-/ssri-5.2.4.tgz";
          sha512 = "2si1dr4dwg0bkvsn38d6rk5raw5lbn2m9giwf8pnhy55wqgxsvv7b1pn252wj166cd8j302a2gwjy00ni473yr1vrxrzmssqs000waj";
        };
      };
      "stack-utils-1.0.1" = {
        name = "stack-utils";
        packageName = "stack-utils";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/stack-utils/-/stack-utils-1.0.1.tgz";
          sha1 = "d4f33ab54e8e38778b0ca5cfd3b3afb12db68620";
        };
      };
      "static-extend-0.1.2" = {
        name = "static-extend";
        packageName = "static-extend";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/static-extend/-/static-extend-0.1.2.tgz";
          sha1 = "60809c39cbff55337226fd5e0b520f341f1fb5c6";
        };
      };
      "statuses-1.3.1" = {
        name = "statuses";
        packageName = "statuses";
        version = "1.3.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/statuses/-/statuses-1.3.1.tgz";
          sha1 = "faf51b9eb74aaef3b3acf4ad5f61abf24cb7b93e";
        };
      };
      "stealthy-require-1.1.1" = {
        name = "stealthy-require";
        packageName = "stealthy-require";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/stealthy-require/-/stealthy-require-1.1.1.tgz";
          sha1 = "35b09875b4ff49f26a777e509b3090a3226bf24b";
        };
      };
      "stream-browserify-2.0.1" = {
        name = "stream-browserify";
        packageName = "stream-browserify";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/stream-browserify/-/stream-browserify-2.0.1.tgz";
          sha1 = "66266ee5f9bdb9940a4e4514cafb43bb71e5c9db";
        };
      };
      "stream-each-1.2.2" = {
        name = "stream-each";
        packageName = "stream-each";
        version = "1.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/stream-each/-/stream-each-1.2.2.tgz";
          sha512 = "2h4ymczmf5aqldga4sj8acqlzc3almazi2vwiv7kx63k28sz1wwkqgzzv1hn47jf49k1x94w25fmmi001h5mj3n6g9in1s6b1n5vkcr";
        };
      };
      "stream-http-2.8.0" = {
        name = "stream-http";
        packageName = "stream-http";
        version = "2.8.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/stream-http/-/stream-http-2.8.0.tgz";
          sha512 = "2ghzil78wsr29z8p1883i0vwx9gpsspha4wvdbpvqzbknrfiavwis010i5a7vy0xx8n486f6kwmjxsk3mg1w4bjy4whvizriz28b4xi";
        };
      };
      "stream-shift-1.0.0" = {
        name = "stream-shift";
        packageName = "stream-shift";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/stream-shift/-/stream-shift-1.0.0.tgz";
          sha1 = "d5c752825e5367e786f78e18e445ea223a155952";
        };
      };
      "strict-uri-encode-1.1.0" = {
        name = "strict-uri-encode";
        packageName = "strict-uri-encode";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strict-uri-encode/-/strict-uri-encode-1.1.0.tgz";
          sha1 = "279b225df1d582b1f54e65addd4352e18faa0713";
        };
      };
      "string-width-1.0.2" = {
        name = "string-width";
        packageName = "string-width";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/string-width/-/string-width-1.0.2.tgz";
          sha1 = "118bdf5b8cdc51a2a7e70d211e07e2b0b9b107d3";
        };
      };
      "string-width-2.1.1" = {
        name = "string-width";
        packageName = "string-width";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/string-width/-/string-width-2.1.1.tgz";
          sha512 = "29s1fqgr4mnhfxwczgdghfmmc1f792m9hysvcjxw2h5lfj8ndf2b6gm02m96qk5m75g4aisijvng4pk618anwbr8i9ay2jyszkqgslw";
        };
      };
      "string_decoder-0.10.31" = {
        name = "string_decoder";
        packageName = "string_decoder";
        version = "0.10.31";
        src = fetchurl {
          url = "https://registry.npmjs.org/string_decoder/-/string_decoder-0.10.31.tgz";
          sha1 = "62e203bc41766c6c28c9fc84301dab1c5310fa94";
        };
      };
      "string_decoder-1.0.3" = {
        name = "string_decoder";
        packageName = "string_decoder";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.0.3.tgz";
          sha512 = "22vw5mmwlyblqc2zyqwl39wyhyahhpiyknim8iz5fk6xi002x777gkswiq8fh297djs5ii4pgrys57wq33hr5zf3xfd0d7kjxkzl0g0";
        };
      };
      "stringstream-0.0.5" = {
        name = "stringstream";
        packageName = "stringstream";
        version = "0.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/stringstream/-/stringstream-0.0.5.tgz";
          sha1 = "4e484cd4de5a0bbbee18e46307710a8a81621878";
        };
      };
      "strip-ansi-0.1.1" = {
        name = "strip-ansi";
        packageName = "strip-ansi";
        version = "0.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-0.1.1.tgz";
          sha1 = "39e8a98d044d150660abe4a6808acf70bb7bc991";
        };
      };
      "strip-ansi-3.0.1" = {
        name = "strip-ansi";
        packageName = "strip-ansi";
        version = "3.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-3.0.1.tgz";
          sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
        };
      };
      "strip-ansi-4.0.0" = {
        name = "strip-ansi";
        packageName = "strip-ansi";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-4.0.0.tgz";
          sha1 = "a8479022eb1ac368a871389b635262c505ee368f";
        };
      };
      "strip-bom-2.0.0" = {
        name = "strip-bom";
        packageName = "strip-bom";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-bom/-/strip-bom-2.0.0.tgz";
          sha1 = "6219a85616520491f35788bdbf1447a99c7e6b0e";
        };
      };
      "strip-bom-3.0.0" = {
        name = "strip-bom";
        packageName = "strip-bom";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-bom/-/strip-bom-3.0.0.tgz";
          sha1 = "2334c18e9c759f7bdd56fdef7e9ae3d588e68ed3";
        };
      };
      "strip-bom-buf-1.0.0" = {
        name = "strip-bom-buf";
        packageName = "strip-bom-buf";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-bom-buf/-/strip-bom-buf-1.0.0.tgz";
          sha1 = "1cb45aaf57530f4caf86c7f75179d2c9a51dd572";
        };
      };
      "strip-eof-1.0.0" = {
        name = "strip-eof";
        packageName = "strip-eof";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-eof/-/strip-eof-1.0.0.tgz";
          sha1 = "bb43ff5598a6eb05d89b59fcd129c983313606bf";
        };
      };
      "strip-indent-1.0.1" = {
        name = "strip-indent";
        packageName = "strip-indent";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-indent/-/strip-indent-1.0.1.tgz";
          sha1 = "0c7962a6adefa7bbd4ac366460a638552ae1a0a2";
        };
      };
      "strip-json-comments-2.0.1" = {
        name = "strip-json-comments";
        packageName = "strip-json-comments";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-2.0.1.tgz";
          sha1 = "3c531942e908c2697c0ec344858c286c7ca0a60a";
        };
      };
      "style-loader-0.19.1" = {
        name = "style-loader";
        packageName = "style-loader";
        version = "0.19.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/style-loader/-/style-loader-0.19.1.tgz";
          sha512 = "0xgklvriylzhm2jn06m89iwzn6aqrvakhs51nyx5fmqql45xw8jaq5cs0ls96dbplasf2hg8zwmv1skyjjb5srd8ahbr3i87253w491";
        };
      };
      "supports-color-2.0.0" = {
        name = "supports-color";
        packageName = "supports-color";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/supports-color/-/supports-color-2.0.0.tgz";
          sha1 = "535d045ce6b6363fa40117084629995e9df324c7";
        };
      };
      "supports-color-3.2.3" = {
        name = "supports-color";
        packageName = "supports-color";
        version = "3.2.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/supports-color/-/supports-color-3.2.3.tgz";
          sha1 = "65ac0504b3954171d8a64946b2ae3cbb8a5f54f6";
        };
      };
      "supports-color-4.5.0" = {
        name = "supports-color";
        packageName = "supports-color";
        version = "4.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/supports-color/-/supports-color-4.5.0.tgz";
          sha1 = "be7a0de484dec5c5cddf8b3d59125044912f635b";
        };
      };
      "supports-color-5.3.0" = {
        name = "supports-color";
        packageName = "supports-color";
        version = "5.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/supports-color/-/supports-color-5.3.0.tgz";
          sha512 = "0v9skvg8c5hgqfsm98p7d7hisk11syjdvl3nxid3ik572hbjwv4vyzws7q0n1yz8mvb1asbk00838fi09hyfskrng54icn8nbag98yi";
        };
      };
      "svgo-0.7.2" = {
        name = "svgo";
        packageName = "svgo";
        version = "0.7.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/svgo/-/svgo-0.7.2.tgz";
          sha1 = "9f5772413952135c6fefbf40afe6a4faa88b4bb5";
        };
      };
      "symbol-observable-0.2.4" = {
        name = "symbol-observable";
        packageName = "symbol-observable";
        version = "0.2.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/symbol-observable/-/symbol-observable-0.2.4.tgz";
          sha1 = "95a83db26186d6af7e7a18dbd9760a2f86d08f40";
        };
      };
      "symbol-observable-1.2.0" = {
        name = "symbol-observable";
        packageName = "symbol-observable";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/symbol-observable/-/symbol-observable-1.2.0.tgz";
          sha512 = "1fxqjwdn747lxzkxrwhbw8arc9czdig47izx5hnzk22z8k7rnw00q8vcpy8v7a3xyamavwgg4mvjkpn2ihjiyjxjnhx2ihiryf39pbv";
        };
      };
      "symbol-tree-3.2.2" = {
        name = "symbol-tree";
        packageName = "symbol-tree";
        version = "3.2.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/symbol-tree/-/symbol-tree-3.2.2.tgz";
          sha1 = "ae27db38f660a7ae2e1c3b7d1bc290819b8519e6";
        };
      };
      "tapable-0.2.8" = {
        name = "tapable";
        packageName = "tapable";
        version = "0.2.8";
        src = fetchurl {
          url = "https://registry.npmjs.org/tapable/-/tapable-0.2.8.tgz";
          sha1 = "99372a5c999bf2df160afc0d74bed4f47948cd22";
        };
      };
      "term-size-1.2.0" = {
        name = "term-size";
        packageName = "term-size";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/term-size/-/term-size-1.2.0.tgz";
          sha1 = "458b83887f288fc56d6fffbfad262e26638efa69";
        };
      };
      "text-table-0.2.0" = {
        name = "text-table";
        packageName = "text-table";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/text-table/-/text-table-0.2.0.tgz";
          sha1 = "7f5ee823ae805207c00af2df4a84ec3fcfa570b4";
        };
      };
      "through2-2.0.3" = {
        name = "through2";
        packageName = "through2";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/through2/-/through2-2.0.3.tgz";
          sha1 = "0004569b37c7c74ba39c43f3ced78d1ad94140be";
        };
      };
      "thunky-1.0.2" = {
        name = "thunky";
        packageName = "thunky";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/thunky/-/thunky-1.0.2.tgz";
          sha1 = "a862e018e3fb1ea2ec3fce5d55605cf57f247371";
        };
      };
      "time-require-0.1.2" = {
        name = "time-require";
        packageName = "time-require";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/time-require/-/time-require-0.1.2.tgz";
          sha1 = "f9e12cb370fc2605e11404582ba54ef5ca2b2d98";
        };
      };
      "time-stamp-2.0.0" = {
        name = "time-stamp";
        packageName = "time-stamp";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/time-stamp/-/time-stamp-2.0.0.tgz";
          sha1 = "95c6a44530e15ba8d6f4a3ecb8c3a3fac46da357";
        };
      };
      "time-zone-1.0.0" = {
        name = "time-zone";
        packageName = "time-zone";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/time-zone/-/time-zone-1.0.0.tgz";
          sha1 = "99c5bf55958966af6d06d83bdf3800dc82faec5d";
        };
      };
      "timed-out-4.0.1" = {
        name = "timed-out";
        packageName = "timed-out";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/timed-out/-/timed-out-4.0.1.tgz";
          sha1 = "f32eacac5a175bea25d7fab565ab3ed8741ef56f";
        };
      };
      "timers-browserify-2.0.6" = {
        name = "timers-browserify";
        packageName = "timers-browserify";
        version = "2.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/timers-browserify/-/timers-browserify-2.0.6.tgz";
          sha512 = "0zvmxvcvmv91k667dy2hzd9a2knvhizxvbx73gcnbi5na3ypc3mldfljw062d7n6y2mf7n2gwwc5wr4wrdih927fxahg8s0hinyf38x";
        };
      };
      "to-array-0.1.4" = {
        name = "to-array";
        packageName = "to-array";
        version = "0.1.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-array/-/to-array-0.1.4.tgz";
          sha1 = "17e6c11f73dd4f3d74cda7a4ff3238e9ad9bf890";
        };
      };
      "to-arraybuffer-1.0.1" = {
        name = "to-arraybuffer";
        packageName = "to-arraybuffer";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-arraybuffer/-/to-arraybuffer-1.0.1.tgz";
          sha1 = "7d229b1fcc637e466ca081180836a7aabff83f43";
        };
      };
      "to-fast-properties-1.0.3" = {
        name = "to-fast-properties";
        packageName = "to-fast-properties";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-1.0.3.tgz";
          sha1 = "b83571fa4d8c25b82e231b06e3a3055de4ca1a47";
        };
      };
      "to-object-path-0.3.0" = {
        name = "to-object-path";
        packageName = "to-object-path";
        version = "0.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-object-path/-/to-object-path-0.3.0.tgz";
          sha1 = "297588b7b0e7e0ac08e04e672f85c1f4999e17af";
        };
      };
      "to-regex-3.0.2" = {
        name = "to-regex";
        packageName = "to-regex";
        version = "3.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-regex/-/to-regex-3.0.2.tgz";
          sha512 = "03lcq1y1ks55lss37m3cx52f8f4wj85rqsxfxrhi3y8rqa0iiny6df8ardg2f742z870v7xw749lcsxh8yplsmbvaig4rrds1w6asqm";
        };
      };
      "to-regex-range-2.1.1" = {
        name = "to-regex-range";
        packageName = "to-regex-range";
        version = "2.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/to-regex-range/-/to-regex-range-2.1.1.tgz";
          sha1 = "7c80c17b9dfebe599e27367e0d4dd5590141db38";
        };
      };
      "toposort-1.0.6" = {
        name = "toposort";
        packageName = "toposort";
        version = "1.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/toposort/-/toposort-1.0.6.tgz";
          sha1 = "c31748e55d210effc00fdcdc7d6e68d7d7bb9cec";
        };
      };
      "tough-cookie-2.3.4" = {
        name = "tough-cookie";
        packageName = "tough-cookie";
        version = "2.3.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.3.4.tgz";
          sha512 = "0ncm6j3cjq1f26mzjf04k9bkw1b08w53s4qa3a11c1bdj4pgnqv1422c1xs5jyy6y1psppjx52fhagq5zkjkgrcpdkxcdiry96r77jd";
        };
      };
      "tr46-1.0.1" = {
        name = "tr46";
        packageName = "tr46";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/tr46/-/tr46-1.0.1.tgz";
          sha1 = "a8b13fd6bfd2489519674ccde55ba3693b706d09";
        };
      };
      "trim-newlines-1.0.0" = {
        name = "trim-newlines";
        packageName = "trim-newlines";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/trim-newlines/-/trim-newlines-1.0.0.tgz";
          sha1 = "5887966bb582a4503a41eb524f7d35011815a613";
        };
      };
      "trim-off-newlines-1.0.1" = {
        name = "trim-off-newlines";
        packageName = "trim-off-newlines";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/trim-off-newlines/-/trim-off-newlines-1.0.1.tgz";
          sha1 = "9f9ba9d9efa8764c387698bcbfeb2c848f11adb3";
        };
      };
      "trim-right-1.0.1" = {
        name = "trim-right";
        packageName = "trim-right";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/trim-right/-/trim-right-1.0.1.tgz";
          sha1 = "cb2e1203067e0c8de1f614094b9fe45704ea6003";
        };
      };
      "tty-browserify-0.0.0" = {
        name = "tty-browserify";
        packageName = "tty-browserify";
        version = "0.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/tty-browserify/-/tty-browserify-0.0.0.tgz";
          sha1 = "a157ba402da24e9bf957f9aa69d524eed42901a6";
        };
      };
      "tunnel-agent-0.6.0" = {
        name = "tunnel-agent";
        packageName = "tunnel-agent";
        version = "0.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz";
          sha1 = "27a5dea06b36b04a0a9966774b290868f0fc40fd";
        };
      };
      "tweetnacl-0.14.5" = {
        name = "tweetnacl";
        packageName = "tweetnacl";
        version = "0.14.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.14.5.tgz";
          sha1 = "5ae68177f192d4456269d108afa93ff8743f4f64";
        };
      };
      "type-check-0.3.2" = {
        name = "type-check";
        packageName = "type-check";
        version = "0.3.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
          sha1 = "5884cab512cf1d355e3fb784f30804b2b520db72";
        };
      };
      "type-is-1.6.16" = {
        name = "type-is";
        packageName = "type-is";
        version = "1.6.16";
        src = fetchurl {
          url = "https://registry.npmjs.org/type-is/-/type-is-1.6.16.tgz";
          sha512 = "3sqj799a59bbpyx9h2nhwjgi776w2jc2pp620b1rgk22fc3czfbxmav4m8kq0ilaqfxx6v1hww5pzgf13bnz6n84fx62qvazszia68x";
        };
      };
      "typedarray-0.0.6" = {
        name = "typedarray";
        packageName = "typedarray";
        version = "0.0.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/typedarray/-/typedarray-0.0.6.tgz";
          sha1 = "867ac74e3864187b1d3d47d996a78ec5c8830777";
        };
      };
      "ua-parser-js-0.7.17" = {
        name = "ua-parser-js";
        packageName = "ua-parser-js";
        version = "0.7.17";
        src = fetchurl {
          url = "https://registry.npmjs.org/ua-parser-js/-/ua-parser-js-0.7.17.tgz";
          sha512 = "39ac4xrr9v9ya7rbn5cz8dss5j3s36yhpj9qrhfxxqzgy1vljns0qfyv7d76lqgdgdbfbrd91kb5x7jlg0fw2r4f3kml0v8xmv545xr";
        };
      };
      "uglify-js-2.8.29" = {
        name = "uglify-js";
        packageName = "uglify-js";
        version = "2.8.29";
        src = fetchurl {
          url = "https://registry.npmjs.org/uglify-js/-/uglify-js-2.8.29.tgz";
          sha1 = "29c5733148057bb4e1f75df35b7a9cb72e6a59dd";
        };
      };
      "uglify-js-3.3.13" = {
        name = "uglify-js";
        packageName = "uglify-js";
        version = "3.3.13";
        src = fetchurl {
          url = "https://registry.npmjs.org/uglify-js/-/uglify-js-3.3.13.tgz";
          sha512 = "1q3k2r6prmhhf44kl90a6p0jk0v94l5k0ihp5qr8wrcvn8bljaj32dhxkfp8nhhy0j3wr5pzqpa6c7512q8gp9k94jm26yfn3yngdzf";
        };
      };
      "uglify-to-browserify-1.0.2" = {
        name = "uglify-to-browserify";
        packageName = "uglify-to-browserify";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/uglify-to-browserify/-/uglify-to-browserify-1.0.2.tgz";
          sha1 = "6e0924d6bda6b5afe349e39a6d632850a0f882b7";
        };
      };
      "uglifyjs-webpack-plugin-0.4.6" = {
        name = "uglifyjs-webpack-plugin";
        packageName = "uglifyjs-webpack-plugin";
        version = "0.4.6";
        src = fetchurl {
          url = "https://registry.npmjs.org/uglifyjs-webpack-plugin/-/uglifyjs-webpack-plugin-0.4.6.tgz";
          sha1 = "b951f4abb6bd617e66f63eb891498e391763e309";
        };
      };
      "uid2-0.0.3" = {
        name = "uid2";
        packageName = "uid2";
        version = "0.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/uid2/-/uid2-0.0.3.tgz";
          sha1 = "483126e11774df2f71b8b639dcd799c376162b82";
        };
      };
      "ultron-1.1.1" = {
        name = "ultron";
        packageName = "ultron";
        version = "1.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/ultron/-/ultron-1.1.1.tgz";
          sha512 = "0x78hsv3jykmjl6qdqlqiz7v5nf06li8b5yvzpj6grnzwbcjch8ngyg55lm8g8mg4znvk7qbryvrr2dxacz3cvyb1nsm64qsw21g0ah";
        };
      };
      "union-value-1.0.0" = {
        name = "union-value";
        packageName = "union-value";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/union-value/-/union-value-1.0.0.tgz";
          sha1 = "5c71c34cb5bad5dcebe3ea0cd08207ba5aa1aea4";
        };
      };
      "uniq-1.0.1" = {
        name = "uniq";
        packageName = "uniq";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/uniq/-/uniq-1.0.1.tgz";
          sha1 = "b31c5ae8254844a3a8281541ce2b04b865a734ff";
        };
      };
      "uniqid-4.1.1" = {
        name = "uniqid";
        packageName = "uniqid";
        version = "4.1.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/uniqid/-/uniqid-4.1.1.tgz";
          sha1 = "89220ddf6b751ae52b5f72484863528596bb84c1";
        };
      };
      "uniqs-2.0.0" = {
        name = "uniqs";
        packageName = "uniqs";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/uniqs/-/uniqs-2.0.0.tgz";
          sha1 = "ffede4b36b25290696e6e165d4a59edb998e6b02";
        };
      };
      "unique-filename-1.1.0" = {
        name = "unique-filename";
        packageName = "unique-filename";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unique-filename/-/unique-filename-1.1.0.tgz";
          sha1 = "d05f2fe4032560871f30e93cbe735eea201514f3";
        };
      };
      "unique-slug-2.0.0" = {
        name = "unique-slug";
        packageName = "unique-slug";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unique-slug/-/unique-slug-2.0.0.tgz";
          sha1 = "db6676e7c7cc0629878ff196097c78855ae9f4ab";
        };
      };
      "unique-string-1.0.0" = {
        name = "unique-string";
        packageName = "unique-string";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unique-string/-/unique-string-1.0.0.tgz";
          sha1 = "9e1057cca851abb93398f8b33ae187b99caec11a";
        };
      };
      "unique-temp-dir-1.0.0" = {
        name = "unique-temp-dir";
        packageName = "unique-temp-dir";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unique-temp-dir/-/unique-temp-dir-1.0.0.tgz";
          sha1 = "6dce95b2681ca003eebfb304a415f9cbabcc5385";
        };
      };
      "units-css-0.4.0" = {
        name = "units-css";
        packageName = "units-css";
        version = "0.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/units-css/-/units-css-0.4.0.tgz";
          sha1 = "d6228653a51983d7c16ff28f8b9dc3b1ffed3a07";
        };
      };
      "unpipe-1.0.0" = {
        name = "unpipe";
        packageName = "unpipe";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz";
          sha1 = "b2bf4ee8514aae6165b4817829d21b2ef49904ec";
        };
      };
      "unset-value-1.0.0" = {
        name = "unset-value";
        packageName = "unset-value";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/unset-value/-/unset-value-1.0.0.tgz";
          sha1 = "8376873f7d2335179ffb1e6fc3a8ed0dfc8ab559";
        };
      };
      "unzip-response-2.0.1" = {
        name = "unzip-response";
        packageName = "unzip-response";
        version = "2.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/unzip-response/-/unzip-response-2.0.1.tgz";
          sha1 = "d2f0f737d16b0615e72a6935ed04214572d56f97";
        };
      };
      "upath-1.0.4" = {
        name = "upath";
        packageName = "upath";
        version = "1.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/upath/-/upath-1.0.4.tgz";
          sha512 = "0xw24ba88hfvwwgniyn17n26av45g1pxqf095231065l4n9dp5w3hyc7azjd8sqyix7pnfx1pmr44fzmwwazkz0ly83cp214g4qk13p";
        };
      };
      "update-notifier-2.3.0" = {
        name = "update-notifier";
        packageName = "update-notifier";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/update-notifier/-/update-notifier-2.3.0.tgz";
          sha1 = "4e8827a6bb915140ab093559d7014e3ebb837451";
        };
      };
      "upper-case-1.1.3" = {
        name = "upper-case";
        packageName = "upper-case";
        version = "1.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/upper-case/-/upper-case-1.1.3.tgz";
          sha1 = "f6b4501c2ec4cdd26ba78be7222961de77621598";
        };
      };
      "urix-0.1.0" = {
        name = "urix";
        packageName = "urix";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/urix/-/urix-0.1.0.tgz";
          sha1 = "da937f7a62e21fec1fd18d49b35c2935067a6c72";
        };
      };
      "url-0.11.0" = {
        name = "url";
        packageName = "url";
        version = "0.11.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/url/-/url-0.11.0.tgz";
          sha1 = "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1";
        };
      };
      "url-loader-0.6.2" = {
        name = "url-loader";
        packageName = "url-loader";
        version = "0.6.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/url-loader/-/url-loader-0.6.2.tgz";
          sha512 = "38ng7sjs8fkmqx2iab2zb4nc24rr67n6wsc1j5sgfjd3nb0q0dklg56h2n5kwdzc8wzw2klp5365s2ips875lvlp5lp1rv76gsryyl7";
        };
      };
      "url-parse-1.0.5" = {
        name = "url-parse";
        packageName = "url-parse";
        version = "1.0.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/url-parse/-/url-parse-1.0.5.tgz";
          sha1 = "0854860422afdcfefeb6c965c662d4800169927b";
        };
      };
      "url-parse-1.2.0" = {
        name = "url-parse";
        packageName = "url-parse";
        version = "1.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/url-parse/-/url-parse-1.2.0.tgz";
          sha512 = "09saicx163rl2p2xnm7hvnwwnj5p2lfxhgrkd1hnxnhpilrs5n9ia7wkqi734sdrswygmzkkz6rf69gh2ii7rngwkx0768zh1nmfg8d";
        };
      };
      "url-parse-lax-1.0.0" = {
        name = "url-parse-lax";
        packageName = "url-parse-lax";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/url-parse-lax/-/url-parse-lax-1.0.0.tgz";
          sha1 = "7af8f303645e9bd79a272e7a14ac68bc0609da73";
        };
      };
      "use-2.0.2" = {
        name = "use";
        packageName = "use";
        version = "2.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/use/-/use-2.0.2.tgz";
          sha1 = "ae28a0d72f93bf22422a18a2e379993112dec8e8";
        };
      };
      "util-0.10.3" = {
        name = "util";
        packageName = "util";
        version = "0.10.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/util/-/util-0.10.3.tgz";
          sha1 = "7afb1afe50805246489e3db7fe0ed379336ac0f9";
        };
      };
      "util-deprecate-1.0.2" = {
        name = "util-deprecate";
        packageName = "util-deprecate";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
          sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
        };
      };
      "utila-0.3.3" = {
        name = "utila";
        packageName = "utila";
        version = "0.3.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/utila/-/utila-0.3.3.tgz";
          sha1 = "d7e8e7d7e309107092b05f8d9688824d633a4226";
        };
      };
      "utila-0.4.0" = {
        name = "utila";
        packageName = "utila";
        version = "0.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/utila/-/utila-0.4.0.tgz";
          sha1 = "8a16a05d445657a3aea5eecc5b12a4fa5379772c";
        };
      };
      "utils-merge-1.0.1" = {
        name = "utils-merge";
        packageName = "utils-merge";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/utils-merge/-/utils-merge-1.0.1.tgz";
          sha1 = "9f95710f50a267947b2ccc124741c1028427e713";
        };
      };
      "uuid-3.2.1" = {
        name = "uuid";
        packageName = "uuid";
        version = "3.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/uuid/-/uuid-3.2.1.tgz";
          sha512 = "0843vl1c974n8kw5kn0kvhvhwk8y8jydr0xkwwl2963xxmkw4ingk6xj9c8m48jw2i95giglxzq5aw5v5mij9kv7fzln8pxav1cr6cd";
        };
      };
      "validate-npm-package-license-3.0.3" = {
        name = "validate-npm-package-license";
        packageName = "validate-npm-package-license";
        version = "3.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/validate-npm-package-license/-/validate-npm-package-license-3.0.3.tgz";
          sha512 = "39caf21ga6s2gvpka90wdngdijarwmkix5fpgh9r94bw14fx1zfnf7j5mprgsdlh8dv0wpwxn65qn9gfwa8n3dxw3iqaj7qf994wxpb";
        };
      };
      "vary-1.1.2" = {
        name = "vary";
        packageName = "vary";
        version = "1.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/vary/-/vary-1.1.2.tgz";
          sha1 = "2299f02c6ded30d4a5961b0b9f74524a18f634fc";
        };
      };
      "vendors-1.0.1" = {
        name = "vendors";
        packageName = "vendors";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/vendors/-/vendors-1.0.1.tgz";
          sha1 = "37ad73c8ee417fb3d580e785312307d274847f22";
        };
      };
      "verror-1.10.0" = {
        name = "verror";
        packageName = "verror";
        version = "1.10.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/verror/-/verror-1.10.0.tgz";
          sha1 = "3a105ca17053af55d6e270c1f8288682e18da400";
        };
      };
      "viewport-dimensions-0.2.0" = {
        name = "viewport-dimensions";
        packageName = "viewport-dimensions";
        version = "0.2.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/viewport-dimensions/-/viewport-dimensions-0.2.0.tgz";
          sha1 = "de740747db5387fd1725f5175e91bac76afdf36c";
        };
      };
      "vm-browserify-0.0.4" = {
        name = "vm-browserify";
        packageName = "vm-browserify";
        version = "0.0.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/vm-browserify/-/vm-browserify-0.0.4.tgz";
          sha1 = "5d7ea45bbef9e4a6ff65f95438e0a87c357d5a73";
        };
      };
      "w3c-hr-time-1.0.1" = {
        name = "w3c-hr-time";
        packageName = "w3c-hr-time";
        version = "1.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/w3c-hr-time/-/w3c-hr-time-1.0.1.tgz";
          sha1 = "82ac2bff63d950ea9e3189a58a65625fedf19045";
        };
      };
      "watchpack-1.5.0" = {
        name = "watchpack";
        packageName = "watchpack";
        version = "1.5.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/watchpack/-/watchpack-1.5.0.tgz";
          sha512 = "003dzsqf9q7awjnkv00gwrqw7s8n29y8nmfcmpsl845j2m7rgxxvvd3gld643c92jfwq9yw7ysbaavw9pq1yc5df8yfxmh1sjj64aa5";
        };
      };
      "waypoints-4.0.1" = {
        name = "waypoints";
        packageName = "waypoints";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/waypoints/-/waypoints-4.0.1.tgz";
          sha1 = "09979a0573810b29627cba4366a284a062ec69c8";
        };
      };
      "wbuf-1.7.2" = {
        name = "wbuf";
        packageName = "wbuf";
        version = "1.7.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/wbuf/-/wbuf-1.7.2.tgz";
          sha1 = "d697b99f1f59512df2751be42769c1580b5801fe";
        };
      };
      "webidl-conversions-4.0.2" = {
        name = "webidl-conversions";
        packageName = "webidl-conversions";
        version = "4.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-4.0.2.tgz";
          sha512 = "15gwgjh9anvzcissfhxy3gki7jxn1dy9vq5rma1sgwkbbra8wbxnvimwalgmy8anm33x56mfp492akzhs0gidwmbnadx0ck3fdq23v1";
        };
      };
      "webpack-3.11.0" = {
        name = "webpack";
        packageName = "webpack";
        version = "3.11.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/webpack/-/webpack-3.11.0.tgz";
          sha512 = "110lxwyg3qdmi7wgs78sjqrknplg29x7mv1qs17s4cyl5dv79axprqcvh7qp2vhjcd6rrv5sn3pq3zgslzl4f4rlh9953xa6mx8ahyy";
        };
      };
      "webpack-dev-middleware-1.12.2" = {
        name = "webpack-dev-middleware";
        packageName = "webpack-dev-middleware";
        version = "1.12.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/webpack-dev-middleware/-/webpack-dev-middleware-1.12.2.tgz";
          sha512 = "3w1xgjc832ngcch11f9a9mpdx6hb9gs66i1qxc2djkh1ssvhmz7r0322hwydy0j20d3bl4lb8d5g90wbi36iljgx46zpjvj5lzylahl";
        };
      };
      "webpack-dev-server-2.11.2" = {
        name = "webpack-dev-server";
        packageName = "webpack-dev-server";
        version = "2.11.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/webpack-dev-server/-/webpack-dev-server-2.11.2.tgz";
          sha512 = "20bd5shvj18c9n3ingxmy372cyx14d2wgs3vk1aams6h8rrafpicv694fdcmpax8akmpyn913smxjn3whx7q5r0cvpqxiyvvrgyicyf";
        };
      };
      "webpack-sources-1.1.0" = {
        name = "webpack-sources";
        packageName = "webpack-sources";
        version = "1.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/webpack-sources/-/webpack-sources-1.1.0.tgz";
          sha512 = "19rska638yxsrpxavydnjckcljiy6ylh63b802hylac396p3mm6j9bj85rhyvi81jk48c33sq580ixwjkbghgwp7cl1i9hgr7bjk9ka";
        };
      };
      "websocket-driver-0.7.0" = {
        name = "websocket-driver";
        packageName = "websocket-driver";
        version = "0.7.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/websocket-driver/-/websocket-driver-0.7.0.tgz";
          sha1 = "0caf9d2d755d93aee049d4bdd0d3fe2cca2a24eb";
        };
      };
      "websocket-extensions-0.1.3" = {
        name = "websocket-extensions";
        packageName = "websocket-extensions";
        version = "0.1.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/websocket-extensions/-/websocket-extensions-0.1.3.tgz";
          sha512 = "0d1n4yv45ibxf72hj7qka3j7v53dwn58savfiyvsppqhhrgg3g648ykk5v7fpb53hz85kj87m4f45r7d5iazx4yqgs381z6qnfd98cy";
        };
      };
      "well-known-symbols-1.0.0" = {
        name = "well-known-symbols";
        packageName = "well-known-symbols";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/well-known-symbols/-/well-known-symbols-1.0.0.tgz";
          sha1 = "73c78ae81a7726a8fa598e2880801c8b16225518";
        };
      };
      "whatwg-encoding-1.0.3" = {
        name = "whatwg-encoding";
        packageName = "whatwg-encoding";
        version = "1.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/whatwg-encoding/-/whatwg-encoding-1.0.3.tgz";
          sha512 = "0zs4j3bywrgl6388qja0fz2rirki31lkvdx50gvx82r0l7f8458g0ikpw3j873vi980p40crd7iqibdz194437v0nnwb2r1lp071c4c";
        };
      };
      "whatwg-fetch-2.0.3" = {
        name = "whatwg-fetch";
        packageName = "whatwg-fetch";
        version = "2.0.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/whatwg-fetch/-/whatwg-fetch-2.0.3.tgz";
          sha1 = "9c84ec2dcf68187ff00bc64e1274b442176e1c84";
        };
      };
      "whatwg-url-6.4.0" = {
        name = "whatwg-url";
        packageName = "whatwg-url";
        version = "6.4.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/whatwg-url/-/whatwg-url-6.4.0.tgz";
          sha512 = "0rf1svigs8z9a5zrrrz1iaaqy83nj0n2ifvw1pihffxis7k1mqf3a6kcwpsd01w94q0s0fypvx6y87sm33z7y16lg2mxnq4ys3rah37";
        };
      };
      "whet.extend-0.9.9" = {
        name = "whet.extend";
        packageName = "whet.extend";
        version = "0.9.9";
        src = fetchurl {
          url = "https://registry.npmjs.org/whet.extend/-/whet.extend-0.9.9.tgz";
          sha1 = "f877d5bf648c97e5aa542fadc16d6a259b9c11a1";
        };
      };
      "which-1.3.0" = {
        name = "which";
        packageName = "which";
        version = "1.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/which/-/which-1.3.0.tgz";
          sha512 = "358cfi3qak701qp5pwkq47n87ca4m8k4lvjl0pdybvmp92nwwd7azzhahy9gy3kg8lqrqdry9l6pl2csflzr0nvwnc3p6asjyi6khn5";
        };
      };
      "which-module-1.0.0" = {
        name = "which-module";
        packageName = "which-module";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/which-module/-/which-module-1.0.0.tgz";
          sha1 = "bba63ca861948994ff307736089e3b96026c2a4f";
        };
      };
      "which-module-2.0.0" = {
        name = "which-module";
        packageName = "which-module";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/which-module/-/which-module-2.0.0.tgz";
          sha1 = "d9ef07dce77b9902b8a3a8fa4b31c3e3f7e6e87a";
        };
      };
      "widest-line-2.0.0" = {
        name = "widest-line";
        packageName = "widest-line";
        version = "2.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/widest-line/-/widest-line-2.0.0.tgz";
          sha1 = "0142a4e8a243f8882c0233aa0e0281aa76152273";
        };
      };
      "window-size-0.1.0" = {
        name = "window-size";
        packageName = "window-size";
        version = "0.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/window-size/-/window-size-0.1.0.tgz";
          sha1 = "5438cd2ea93b202efa3a19fe8887aee7c94f9c9d";
        };
      };
      "wordwrap-0.0.2" = {
        name = "wordwrap";
        packageName = "wordwrap";
        version = "0.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/wordwrap/-/wordwrap-0.0.2.tgz";
          sha1 = "b79669bb42ecb409f83d583cad52ca17eaa1643f";
        };
      };
      "wordwrap-1.0.0" = {
        name = "wordwrap";
        packageName = "wordwrap";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/wordwrap/-/wordwrap-1.0.0.tgz";
          sha1 = "27584810891456a4171c8d0226441ade90cbcaeb";
        };
      };
      "wrap-ansi-2.1.0" = {
        name = "wrap-ansi";
        packageName = "wrap-ansi";
        version = "2.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-2.1.0.tgz";
          sha1 = "d8fc3d284dd05794fe84973caecdd1cf824fdd85";
        };
      };
      "wrappy-1.0.2" = {
        name = "wrappy";
        packageName = "wrappy";
        version = "1.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
          sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
        };
      };
      "write-file-atomic-1.3.4" = {
        name = "write-file-atomic";
        packageName = "write-file-atomic";
        version = "1.3.4";
        src = fetchurl {
          url = "https://registry.npmjs.org/write-file-atomic/-/write-file-atomic-1.3.4.tgz";
          sha1 = "f807a4f0b1d9e913ae7a48112e6cc3af1991b45f";
        };
      };
      "write-file-atomic-2.3.0" = {
        name = "write-file-atomic";
        packageName = "write-file-atomic";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/write-file-atomic/-/write-file-atomic-2.3.0.tgz";
          sha512 = "2sgqxmcqzjd7nq9gjh6jz7vfb0gs0ag4jvqzdq93afq3bw3jrm88mhxql9sryyb04f3ipw5jkgjfiigsmdwlz9fgsnnm3cxhcmxxqy6";
        };
      };
      "write-json-file-2.3.0" = {
        name = "write-json-file";
        packageName = "write-json-file";
        version = "2.3.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/write-json-file/-/write-json-file-2.3.0.tgz";
          sha1 = "2b64c8a33004d54b8698c76d585a77ceb61da32f";
        };
      };
      "write-pkg-3.1.0" = {
        name = "write-pkg";
        packageName = "write-pkg";
        version = "3.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/write-pkg/-/write-pkg-3.1.0.tgz";
          sha1 = "030a9994cc9993d25b4e75a9f1a1923607291ce9";
        };
      };
      "ws-3.3.3" = {
        name = "ws";
        packageName = "ws";
        version = "3.3.3";
        src = fetchurl {
          url = "https://registry.npmjs.org/ws/-/ws-3.3.3.tgz";
          sha512 = "2887c18dlvnvc62pqgwhihzxnnj9mzbnjqa0gqg3n94k5b6fx6nm1wggisy2bg3mi7dl81vk11i49wl319yfvh255w2nrbhydmqnxcy";
        };
      };
      "ws-4.1.0" = {
        name = "ws";
        packageName = "ws";
        version = "4.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/ws/-/ws-4.1.0.tgz";
          sha512 = "1ldy8hddsvy7lb045cx4jrnx09962j98zp7y16f64gkw8z99ww61w91mjhrm85bqpsf3b158yhfh6yf01g1a2zrgm6v9bkx87r7ys34";
        };
      };
      "xdg-basedir-3.0.0" = {
        name = "xdg-basedir";
        packageName = "xdg-basedir";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/xdg-basedir/-/xdg-basedir-3.0.0.tgz";
          sha1 = "496b2cc109eca8dbacfe2dc72b603c17c5870ad4";
        };
      };
      "xml-char-classes-1.0.0" = {
        name = "xml-char-classes";
        packageName = "xml-char-classes";
        version = "1.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/xml-char-classes/-/xml-char-classes-1.0.0.tgz";
          sha1 = "64657848a20ffc5df583a42ad8a277b4512bbc4d";
        };
      };
      "xml-name-validator-3.0.0" = {
        name = "xml-name-validator";
        packageName = "xml-name-validator";
        version = "3.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/xml-name-validator/-/xml-name-validator-3.0.0.tgz";
          sha512 = "0zzh3xf5vk49fghwrf83qvq6v98qin1brnwd7yyh7qs05gc1w3xni0w17sj33534jrkm2za7wkm4q5rhgpchds54i5grcj3vjk99403";
        };
      };
      "xmlhttprequest-ssl-1.5.5" = {
        name = "xmlhttprequest-ssl";
        packageName = "xmlhttprequest-ssl";
        version = "1.5.5";
        src = fetchurl {
          url = "https://registry.npmjs.org/xmlhttprequest-ssl/-/xmlhttprequest-ssl-1.5.5.tgz";
          sha1 = "c2876b06168aadc40e57d97e81191ac8f4398b3e";
        };
      };
      "xtend-4.0.1" = {
        name = "xtend";
        packageName = "xtend";
        version = "4.0.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/xtend/-/xtend-4.0.1.tgz";
          sha1 = "a5c6d532be656e23db820efb943a1f04998d63af";
        };
      };
      "y18n-3.2.1" = {
        name = "y18n";
        packageName = "y18n";
        version = "3.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/y18n/-/y18n-3.2.1.tgz";
          sha1 = "6d15fba884c08679c0d77e88e7759e811e07fa41";
        };
      };
      "y18n-4.0.0" = {
        name = "y18n";
        packageName = "y18n";
        version = "4.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/y18n/-/y18n-4.0.0.tgz";
          sha512 = "3zj75gvpcgiphxpci4ji1znykk9n4cs0aw3dd6inwdvkmxyqn2483vya70lssjwq8alspnpw88vgii21fdrcn2vmfyppzgf4mkvzm5g";
        };
      };
      "yallist-2.1.2" = {
        name = "yallist";
        packageName = "yallist";
        version = "2.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/yallist/-/yallist-2.1.2.tgz";
          sha1 = "1c11f9218f076089a47dd512f93c6699a6a81d52";
        };
      };
      "yargs-11.1.0" = {
        name = "yargs";
        packageName = "yargs";
        version = "11.1.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs/-/yargs-11.1.0.tgz";
          sha512 = "3j5awbfcbh8ik0kz01mycydpi1bz9fg70xc66lk1r1qvrs5x41i2w8nvgj0aip7z9vypcsxks76z75sz4lr6z3ida9c04inkvsbl19p";
        };
      };
      "yargs-3.10.0" = {
        name = "yargs";
        packageName = "yargs";
        version = "3.10.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs/-/yargs-3.10.0.tgz";
          sha1 = "f7ee7bd857dd7c1d2d38c0e74efbd681d1431fd1";
        };
      };
      "yargs-6.6.0" = {
        name = "yargs";
        packageName = "yargs";
        version = "6.6.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs/-/yargs-6.6.0.tgz";
          sha1 = "782ec21ef403345f830a808ca3d513af56065208";
        };
      };
      "yargs-8.0.2" = {
        name = "yargs";
        packageName = "yargs";
        version = "8.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs/-/yargs-8.0.2.tgz";
          sha1 = "6299a9055b1cefc969ff7e79c1d918dceb22c360";
        };
      };
      "yargs-parser-4.2.1" = {
        name = "yargs-parser";
        packageName = "yargs-parser";
        version = "4.2.1";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-4.2.1.tgz";
          sha1 = "29cceac0dc4f03c6c87b4a9f217dd18c9f74871c";
        };
      };
      "yargs-parser-7.0.0" = {
        name = "yargs-parser";
        packageName = "yargs-parser";
        version = "7.0.0";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-7.0.0.tgz";
          sha1 = "8d0ac42f16ea55debd332caf4c4038b3e3f5dfd9";
        };
      };
      "yargs-parser-9.0.2" = {
        name = "yargs-parser";
        packageName = "yargs-parser";
        version = "9.0.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-9.0.2.tgz";
          sha1 = "9ccf6a43460fe4ed40a9bb68f48d43b8a68cc077";
        };
      };
      "yeast-0.1.2" = {
        name = "yeast";
        packageName = "yeast";
        version = "0.1.2";
        src = fetchurl {
          url = "https://registry.npmjs.org/yeast/-/yeast-0.1.2.tgz";
          sha1 = "008e06d8094320c372dbc2f8ed76a0ca6c8ac419";
        };
      };
    };
    args = {
      name = "cardano-sl-explorer";
      packageName = "cardano-sl-explorer";
      version = "0.2.0";
      inherit src;
      dependencies = [
        sources."@ava/babel-plugin-throws-helper-2.0.0"
        (sources."@ava/babel-preset-stage-4-1.1.0" // {
          dependencies = [
            sources."md5-hex-1.3.0"
            sources."package-hash-1.2.0"
          ];
        })
        sources."@ava/babel-preset-transform-test-files-3.0.0"
        sources."@ava/write-file-atomic-2.2.0"
        sources."@concordance/react-1.0.0"
        sources."abab-1.0.4"
        sources."accepts-1.3.5"
        sources."acorn-5.5.3"
        (sources."acorn-dynamic-import-2.0.2" // {
          dependencies = [
            sources."acorn-4.0.13"
          ];
        })
        sources."acorn-globals-4.1.0"
        sources."after-0.8.2"
        sources."ajv-5.5.2"
        sources."ajv-keywords-3.1.0"
        sources."align-text-0.1.4"
        sources."alphanum-sort-1.0.2"
        sources."animate.css-3.6.1"
        sources."ansi-align-2.0.0"
        sources."ansi-escapes-2.0.0"
        sources."ansi-html-0.0.7"
        sources."ansi-regex-2.1.1"
        sources."ansi-styles-3.2.1"
        sources."anymatch-1.3.2"
        sources."aproba-1.2.0"
        sources."argparse-1.0.10"
        sources."arr-diff-2.0.0"
        sources."arr-exclude-1.0.0"
        sources."arr-flatten-1.1.0"
        sources."arr-union-3.1.0"
        sources."array-differ-1.0.0"
        sources."array-equal-1.0.0"
        sources."array-find-index-1.0.2"
        sources."array-flatten-2.1.1"
        sources."array-includes-3.0.3"
        sources."array-union-1.0.2"
        sources."array-uniq-1.0.3"
        sources."array-unique-0.2.1"
        sources."arraybuffer.slice-0.0.7"
        sources."arrify-1.0.1"
        sources."asap-2.0.6"
        sources."asn1-0.2.3"
        sources."asn1.js-4.10.1"
        sources."assert-1.4.1"
        sources."assert-plus-1.0.0"
        sources."assign-symbols-1.0.0"
        sources."async-2.6.0"
        sources."async-each-1.0.1"
        sources."async-limiter-1.0.0"
        sources."asynckit-0.4.0"
        sources."atob-2.0.3"
        sources."auto-bind-1.2.0"
        sources."autoprefixer-7.2.6"
        (sources."ava-0.23.0" // {
          dependencies = [
            sources."find-up-2.1.0"
            sources."load-json-file-4.0.0"
            sources."path-type-1.1.0"
            sources."pify-3.0.0"
            sources."strip-bom-3.0.0"
            sources."write-file-atomic-2.3.0"
          ];
        })
        sources."ava-init-0.2.1"
        sources."aws-sign2-0.7.0"
        sources."aws4-1.6.0"
        sources."babel-code-frame-6.26.0"
        (sources."babel-core-6.26.0" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            sources."chalk-1.1.3"
            sources."source-map-0.5.7"
            sources."supports-color-2.0.0"
          ];
        })
        sources."babel-generator-6.26.1"
        sources."babel-helper-bindify-decorators-6.24.1"
        sources."babel-helper-builder-binary-assignment-operator-visitor-6.24.1"
        sources."babel-helper-builder-react-jsx-6.26.0"
        sources."babel-helper-call-delegate-6.24.1"
        sources."babel-helper-define-map-6.26.0"
        sources."babel-helper-explode-assignable-expression-6.24.1"
        sources."babel-helper-explode-class-6.24.1"
        sources."babel-helper-function-name-6.24.1"
        sources."babel-helper-get-function-arity-6.24.1"
        sources."babel-helper-hoist-variables-6.24.1"
        sources."babel-helper-optimise-call-expression-6.24.1"
        sources."babel-helper-regex-6.26.0"
        sources."babel-helper-remap-async-to-generator-6.24.1"
        sources."babel-helper-replace-supers-6.24.1"
        sources."babel-helpers-6.24.1"
        sources."babel-loader-7.1.4"
        sources."babel-messages-6.23.0"
        sources."babel-plugin-check-es2015-constants-6.22.0"
        sources."babel-plugin-espower-2.4.0"
        sources."babel-plugin-syntax-async-functions-6.13.0"
        sources."babel-plugin-syntax-async-generators-6.13.0"
        sources."babel-plugin-syntax-class-properties-6.13.0"
        sources."babel-plugin-syntax-decorators-6.13.0"
        sources."babel-plugin-syntax-dynamic-import-6.18.0"
        sources."babel-plugin-syntax-exponentiation-operator-6.13.0"
        sources."babel-plugin-syntax-flow-6.18.0"
        sources."babel-plugin-syntax-jsx-6.18.0"
        sources."babel-plugin-syntax-object-rest-spread-6.13.0"
        sources."babel-plugin-syntax-trailing-function-commas-6.22.0"
        sources."babel-plugin-transform-async-generator-functions-6.24.1"
        sources."babel-plugin-transform-async-to-generator-6.24.1"
        sources."babel-plugin-transform-class-properties-6.24.1"
        sources."babel-plugin-transform-decorators-6.24.1"
        sources."babel-plugin-transform-es2015-arrow-functions-6.22.0"
        sources."babel-plugin-transform-es2015-block-scoped-functions-6.22.0"
        sources."babel-plugin-transform-es2015-block-scoping-6.26.0"
        sources."babel-plugin-transform-es2015-classes-6.24.1"
        sources."babel-plugin-transform-es2015-computed-properties-6.24.1"
        sources."babel-plugin-transform-es2015-destructuring-6.23.0"
        sources."babel-plugin-transform-es2015-duplicate-keys-6.24.1"
        sources."babel-plugin-transform-es2015-for-of-6.23.0"
        sources."babel-plugin-transform-es2015-function-name-6.24.1"
        sources."babel-plugin-transform-es2015-literals-6.22.0"
        sources."babel-plugin-transform-es2015-modules-amd-6.24.1"
        sources."babel-plugin-transform-es2015-modules-commonjs-6.26.0"
        sources."babel-plugin-transform-es2015-modules-systemjs-6.24.1"
        sources."babel-plugin-transform-es2015-modules-umd-6.24.1"
        sources."babel-plugin-transform-es2015-object-super-6.24.1"
        sources."babel-plugin-transform-es2015-parameters-6.24.1"
        sources."babel-plugin-transform-es2015-shorthand-properties-6.24.1"
        sources."babel-plugin-transform-es2015-spread-6.22.0"
        sources."babel-plugin-transform-es2015-sticky-regex-6.24.1"
        sources."babel-plugin-transform-es2015-template-literals-6.22.0"
        sources."babel-plugin-transform-es2015-typeof-symbol-6.23.0"
        sources."babel-plugin-transform-es2015-unicode-regex-6.24.1"
        sources."babel-plugin-transform-exponentiation-operator-6.24.1"
        sources."babel-plugin-transform-flow-strip-types-6.22.0"
        sources."babel-plugin-transform-object-rest-spread-6.26.0"
        sources."babel-plugin-transform-react-display-name-6.25.0"
        sources."babel-plugin-transform-react-jsx-6.24.1"
        sources."babel-plugin-transform-react-jsx-self-6.22.0"
        sources."babel-plugin-transform-react-jsx-source-6.22.0"
        sources."babel-plugin-transform-regenerator-6.26.0"
        sources."babel-plugin-transform-strict-mode-6.24.1"
        sources."babel-polyfill-6.26.0"
        (sources."babel-preset-es2015-6.24.1" // {
          dependencies = [
            sources."jsesc-0.5.0"
          ];
        })
        sources."babel-preset-flow-6.23.0"
        sources."babel-preset-react-6.24.1"
        sources."babel-preset-stage-2-6.24.1"
        sources."babel-preset-stage-3-6.24.1"
        sources."babel-register-6.26.0"
        (sources."babel-runtime-6.26.0" // {
          dependencies = [
            sources."regenerator-runtime-0.11.1"
          ];
        })
        sources."babel-template-6.26.0"
        sources."babel-traverse-6.26.0"
        sources."babel-types-6.26.0"
        sources."babylon-6.18.0"
        sources."backo2-1.0.2"
        sources."balanced-match-1.0.0"
        (sources."base-0.11.2" // {
          dependencies = [
            (sources."define-property-1.0.0" // {
              dependencies = [
                sources."kind-of-6.0.2"
              ];
            })
            sources."kind-of-3.2.2"
          ];
        })
        sources."base64-arraybuffer-0.1.5"
        sources."base64-js-1.2.3"
        sources."batch-0.6.1"
        sources."bcrypt-pbkdf-1.0.1"
        sources."better-assert-1.0.2"
        sources."big.js-3.2.0"
        sources."bignumber.js-4.1.0"
        sources."binary-extensions-1.11.0"
        sources."blob-0.0.4"
        sources."bluebird-3.5.1"
        sources."bn.js-4.11.8"
        (sources."body-parser-1.18.2" // {
          dependencies = [
            sources."setprototypeof-1.0.3"
          ];
        })
        sources."bonjour-3.5.0"
        sources."boolbase-1.0.0"
        sources."boom-4.3.1"
        sources."bower-1.8.2"
        sources."boxen-1.3.0"
        sources."brace-expansion-1.1.11"
        (sources."braces-1.8.5" // {
          dependencies = [
            sources."kind-of-4.0.0"
          ];
        })
        sources."brorand-1.1.0"
        sources."browser-process-hrtime-0.1.2"
        sources."browserify-aes-1.1.1"
        sources."browserify-cipher-1.0.0"
        sources."browserify-des-1.0.0"
        sources."browserify-rsa-4.0.1"
        sources."browserify-sign-4.0.4"
        sources."browserify-zlib-0.2.0"
        sources."browserslist-2.11.3"
        sources."buf-compare-1.0.1"
        sources."buffer-4.9.1"
        sources."buffer-indexof-1.1.1"
        sources."buffer-xor-1.0.3"
        sources."builtin-modules-1.1.1"
        sources."builtin-status-codes-3.0.0"
        sources."bytes-3.0.0"
        sources."cacache-10.0.4"
        sources."cache-base-1.0.1"
        (sources."caching-transform-1.0.1" // {
          dependencies = [
            sources."md5-hex-1.3.0"
          ];
        })
        sources."call-matcher-1.0.1"
        sources."call-signature-0.0.2"
        sources."callsite-1.0.0"
        sources."camel-case-3.0.0"
        sources."camelcase-2.1.1"
        sources."camelcase-keys-2.1.0"
        sources."caniuse-api-1.6.1"
        sources."caniuse-db-1.0.30000813"
        sources."caniuse-lite-1.0.30000813"
        sources."capture-stack-trace-1.0.0"
        sources."caseless-0.12.0"
        sources."center-align-0.1.3"
        sources."chalk-2.3.2"
        sources."chokidar-1.7.0"
        sources."chownr-1.0.1"
        sources."ci-info-1.1.2"
        sources."cipher-base-1.0.4"
        sources."clap-1.2.3"
        (sources."class-utils-0.3.6" // {
          dependencies = [
            sources."define-property-0.2.5"
          ];
        })
        sources."clean-css-4.1.11"
        sources."clean-stack-1.3.0"
        sources."clean-yaml-object-0.1.0"
        sources."cli-boxes-1.0.0"
        sources."cli-cursor-2.1.0"
        sources."cli-spinners-1.1.0"
        sources."cli-truncate-1.1.0"
        sources."cliui-4.0.0"
        sources."clone-1.0.3"
        sources."co-4.6.0"
        sources."co-with-promise-4.6.0"
        sources."coa-1.0.4"
        sources."code-excerpt-2.1.1"
        sources."code-point-at-1.1.0"
        sources."collection-visit-1.0.0"
        sources."color-0.11.4"
        sources."color-convert-1.9.1"
        sources."color-name-1.1.3"
        sources."color-string-0.3.0"
        sources."colormin-1.1.2"
        sources."colors-1.1.2"
        sources."combined-stream-1.0.6"
        sources."commander-2.14.1"
        sources."common-path-prefix-1.0.0"
        sources."commondir-1.0.1"
        sources."component-bind-1.0.0"
        sources."component-emitter-1.2.1"
        sources."component-inherit-0.0.3"
        sources."compressible-2.0.13"
        (sources."compression-1.7.2" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."concat-map-0.0.1"
        sources."concat-stream-1.6.1"
        sources."concordance-3.0.0"
        sources."configstore-3.1.1"
        sources."connect-history-api-fallback-1.5.0"
        sources."console-browserify-1.1.0"
        sources."constants-browserify-1.0.0"
        sources."content-disposition-0.5.2"
        sources."content-type-1.0.4"
        sources."content-type-parser-1.0.2"
        sources."convert-source-map-1.5.1"
        sources."convert-to-spaces-1.0.2"
        sources."cookie-0.3.1"
        sources."cookie-signature-1.0.6"
        sources."copy-concurrently-1.0.5"
        sources."copy-descriptor-0.1.1"
        (sources."copy-webpack-plugin-4.5.1" // {
          dependencies = [
            sources."isarray-1.0.0"
          ];
        })
        sources."core-assert-0.2.1"
        sources."core-js-2.5.3"
        sources."core-util-is-1.0.2"
        sources."cosmiconfig-2.2.2"
        sources."create-ecdh-4.0.0"
        sources."create-error-class-3.0.2"
        sources."create-hash-1.1.3"
        sources."create-hmac-1.1.6"
        (sources."create-react-class-15.6.3" // {
          dependencies = [
            sources."core-js-1.2.7"
          ];
        })
        sources."cross-spawn-5.1.0"
        (sources."cryptiles-3.1.2" // {
          dependencies = [
            sources."boom-5.2.0"
          ];
        })
        sources."crypto-browserify-3.12.0"
        sources."crypto-random-string-1.0.0"
        sources."css-color-function-1.3.3"
        sources."css-color-names-0.0.4"
        (sources."css-loader-0.28.10" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."has-flag-1.0.0"
            sources."postcss-5.2.18"
            sources."regexpu-core-1.0.0"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        sources."css-select-1.2.0"
        sources."css-selector-tokenizer-0.7.0"
        sources."css-unit-converter-1.1.1"
        sources."css-what-2.1.0"
        sources."cssesc-0.1.0"
        (sources."cssnano-3.10.0" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            sources."autoprefixer-6.7.7"
            sources."balanced-match-0.4.2"
            sources."browserslist-1.7.7"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."has-flag-1.0.0"
            sources."postcss-5.2.18"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        sources."csso-2.3.2"
        sources."cssom-0.3.2"
        sources."cssstyle-0.2.37"
        sources."currently-unhandled-0.4.1"
        sources."cyclist-0.2.2"
        sources."d-1.0.0"
        sources."dargs-5.1.0"
        sources."dashdash-1.14.1"
        sources."date-now-0.1.4"
        sources."date-time-2.1.0"
        sources."debug-2.6.9"
        sources."decamelize-1.2.0"
        sources."decode-uri-component-0.2.0"
        sources."deep-equal-1.0.1"
        sources."deep-extend-0.4.2"
        sources."deep-is-0.1.3"
        sources."define-properties-1.1.2"
        sources."define-property-2.0.2"
        sources."defined-1.0.0"
        sources."del-3.0.0"
        sources."delayed-stream-1.0.0"
        sources."depd-1.1.2"
        sources."des.js-1.0.0"
        sources."destroy-1.0.4"
        sources."detect-indent-4.0.0"
        sources."detect-node-2.0.3"
        sources."diffie-hellman-5.0.2"
        sources."dir-glob-2.0.0"
        sources."dns-equal-1.0.0"
        sources."dns-packet-1.3.1"
        sources."dns-txt-2.0.2"
        sources."dom-converter-0.1.4"
        (sources."dom-serializer-0.1.0" // {
          dependencies = [
            sources."domelementtype-1.1.3"
          ];
        })
        sources."domain-browser-1.2.0"
        sources."domelementtype-1.3.0"
        sources."domexception-1.0.1"
        sources."domhandler-2.1.0"
        sources."domutils-1.5.1"
        sources."dot-prop-4.2.0"
        sources."duplexer3-0.1.4"
        sources."duplexify-3.5.4"
        sources."ecc-jsbn-0.1.1"
        sources."ee-first-1.1.1"
        sources."electron-to-chromium-1.3.36"
        sources."elliptic-6.4.0"
        sources."emojis-list-2.1.0"
        sources."empower-core-0.6.2"
        sources."encodeurl-1.0.2"
        sources."encoding-0.1.12"
        sources."end-of-stream-1.4.1"
        (sources."engine.io-client-3.1.6" // {
          dependencies = [
            sources."debug-3.1.0"
          ];
        })
        sources."engine.io-parser-2.1.2"
        sources."enhanced-resolve-3.4.1"
        sources."entities-1.1.1"
        sources."equal-length-1.0.1"
        sources."err-code-1.1.2"
        sources."errno-0.1.7"
        sources."error-ex-1.3.1"
        sources."es-abstract-1.10.0"
        sources."es-to-primitive-1.1.1"
        sources."es5-ext-0.10.40"
        sources."es6-error-4.1.1"
        sources."es6-iterator-2.0.3"
        sources."es6-map-0.1.5"
        sources."es6-set-0.1.5"
        sources."es6-symbol-3.1.1"
        sources."es6-weak-map-2.0.2"
        sources."escape-html-1.0.3"
        sources."escape-string-regexp-1.0.5"
        sources."escodegen-1.9.1"
        sources."escope-3.6.0"
        sources."espower-location-detector-1.0.0"
        sources."esprima-3.1.3"
        sources."espurify-1.7.0"
        sources."esrecurse-4.2.1"
        sources."estraverse-4.2.0"
        sources."esutils-2.0.2"
        sources."etag-1.8.1"
        sources."event-emitter-0.3.5"
        sources."eventemitter3-1.2.0"
        sources."events-1.1.1"
        (sources."eventsource-0.1.6" // {
          dependencies = [
            sources."url-parse-1.0.5"
          ];
        })
        sources."evp_bytestokey-1.0.3"
        sources."execa-0.7.0"
        sources."expand-brackets-0.1.5"
        sources."expand-range-1.8.2"
        (sources."express-4.16.2" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."extend-3.0.1"
        sources."extend-shallow-3.0.2"
        sources."extglob-0.3.2"
        sources."extract-text-webpack-plugin-3.0.2"
        sources."extsprintf-1.3.0"
        sources."fast-deep-equal-1.1.0"
        sources."fast-diff-1.1.2"
        sources."fast-json-stable-stringify-2.0.0"
        sources."fast-levenshtein-2.0.6"
        sources."fastparse-1.1.1"
        sources."faye-websocket-0.10.0"
        sources."fbjs-0.8.16"
        sources."figures-2.0.0"
        (sources."file-loader-1.1.11" // {
          dependencies = [
            sources."ajv-6.2.1"
            sources."schema-utils-0.4.5"
          ];
        })
        sources."filename-regex-2.0.1"
        sources."fill-range-2.2.3"
        sources."finalhandler-1.1.0"
        sources."find-cache-dir-1.0.0"
        sources."find-up-2.1.0"
        sources."flatten-1.0.2"
        sources."flush-write-stream-1.0.2"
        sources."fn-name-2.0.1"
        sources."for-in-1.0.2"
        sources."for-own-0.1.5"
        sources."foreach-2.0.5"
        sources."forever-agent-0.6.1"
        sources."form-data-2.3.2"
        sources."forwarded-0.1.2"
        sources."fragment-cache-0.2.1"
        sources."fresh-0.5.2"
        sources."from2-2.3.0"
        sources."fs-write-stream-atomic-1.0.10"
        sources."fs.realpath-1.0.0"
        sources."fsevents-1.1.3"
        sources."function-bind-1.1.1"
        sources."function-name-support-0.2.0"
        sources."get-caller-file-1.0.2"
        sources."get-port-3.2.0"
        sources."get-stdin-4.0.1"
        sources."get-stream-3.0.0"
        sources."get-value-2.0.6"
        sources."getpass-0.1.7"
        sources."git-revision-webpack-plugin-2.5.1"
        sources."glob-7.1.2"
        sources."glob-base-0.3.0"
        sources."glob-parent-2.0.0"
        sources."global-dirs-0.1.1"
        sources."globals-9.18.0"
        sources."globby-7.1.1"
        sources."got-6.7.1"
        sources."graceful-fs-4.1.11"
        sources."handle-thing-1.2.5"
        sources."har-schema-2.0.0"
        sources."har-validator-5.0.3"
        sources."has-1.0.1"
        sources."has-ansi-2.0.0"
        sources."has-binary2-1.0.2"
        sources."has-color-0.1.7"
        sources."has-cors-1.1.0"
        sources."has-flag-3.0.0"
        sources."has-value-1.0.0"
        sources."has-values-1.0.0"
        sources."has-yarn-1.0.0"
        sources."hash-base-3.0.4"
        sources."hash.js-1.1.3"
        sources."hawk-6.0.2"
        sources."he-1.1.1"
        sources."hmac-drbg-1.0.1"
        sources."hoek-4.2.1"
        sources."home-or-tmp-2.0.0"
        sources."hosted-git-info-2.6.0"
        sources."hpack.js-2.1.6"
        sources."html-comment-regex-1.1.1"
        sources."html-encoding-sniffer-1.0.2"
        sources."html-entities-1.2.1"
        (sources."html-minifier-3.5.10" // {
          dependencies = [
            sources."source-map-0.6.1"
          ];
        })
        (sources."html-webpack-plugin-2.30.1" // {
          dependencies = [
            sources."domutils-1.1.6"
            sources."isarray-0.0.1"
            sources."loader-utils-0.2.17"
            sources."readable-stream-1.0.34"
            sources."source-map-0.5.7"
            sources."string_decoder-0.10.31"
          ];
        })
        sources."htmlparser2-3.3.0"
        sources."http-deceiver-1.2.7"
        (sources."http-errors-1.6.2" // {
          dependencies = [
            sources."depd-1.1.1"
          ];
        })
        sources."http-parser-js-0.4.11"
        sources."http-proxy-1.16.2"
        (sources."http-proxy-middleware-0.17.4" // {
          dependencies = [
            sources."arr-diff-2.0.0"
            sources."array-unique-0.2.1"
            sources."braces-1.8.5"
            sources."expand-brackets-0.1.5"
            sources."extglob-0.3.2"
            sources."kind-of-3.2.2"
            (sources."micromatch-2.3.11" // {
              dependencies = [
                sources."is-glob-2.0.1"
              ];
            })
          ];
        })
        sources."http-signature-1.2.0"
        sources."https-browserify-1.0.0"
        sources."hullabaloo-config-manager-1.1.1"
        sources."iconv-lite-0.4.19"
        sources."icss-replace-symbols-1.1.0"
        (sources."icss-utils-2.1.0" // {
          dependencies = [
            sources."postcss-6.0.19"
          ];
        })
        sources."ieee754-1.1.8"
        sources."iferr-0.1.5"
        sources."ignore-3.3.7"
        sources."ignore-by-default-1.0.1"
        sources."import-lazy-2.1.0"
        sources."import-local-0.1.1"
        sources."imurmurhash-0.1.4"
        sources."indent-string-3.2.0"
        sources."indexes-of-1.0.1"
        sources."indexof-0.0.1"
        sources."inflight-1.0.6"
        sources."inherits-2.0.3"
        sources."ini-1.3.5"
        sources."internal-ip-1.2.0"
        sources."interpret-1.1.0"
        sources."invariant-2.2.3"
        sources."invert-kv-1.0.0"
        sources."ip-1.1.5"
        sources."ipaddr.js-1.6.0"
        sources."irregular-plurals-1.4.0"
        sources."is-absolute-url-2.1.0"
        sources."is-accessor-descriptor-1.0.0"
        sources."is-arrayish-0.2.1"
        sources."is-binary-path-1.0.1"
        sources."is-buffer-1.1.6"
        sources."is-builtin-module-1.0.0"
        sources."is-callable-1.1.3"
        sources."is-ci-1.1.0"
        sources."is-data-descriptor-1.0.0"
        sources."is-date-object-1.0.1"
        sources."is-descriptor-1.0.2"
        sources."is-directory-0.3.1"
        sources."is-dotfile-1.0.3"
        sources."is-equal-shallow-0.1.3"
        sources."is-error-2.2.1"
        sources."is-extendable-0.1.1"
        sources."is-extglob-2.1.1"
        sources."is-finite-1.0.2"
        sources."is-fullwidth-code-point-2.0.0"
        sources."is-generator-fn-1.0.0"
        sources."is-glob-4.0.0"
        sources."is-installed-globally-0.1.0"
        sources."is-npm-1.0.0"
        sources."is-number-2.1.0"
        sources."is-obj-1.0.1"
        sources."is-observable-0.2.0"
        sources."is-odd-2.0.0"
        sources."is-path-cwd-1.0.0"
        sources."is-path-in-cwd-1.0.0"
        sources."is-path-inside-1.0.1"
        sources."is-plain-obj-1.1.0"
        sources."is-plain-object-2.0.4"
        sources."is-posix-bracket-0.1.1"
        sources."is-primitive-2.0.0"
        sources."is-promise-2.1.0"
        sources."is-redirect-1.0.0"
        sources."is-regex-1.0.4"
        sources."is-retry-allowed-1.1.0"
        sources."is-stream-1.1.0"
        sources."is-svg-2.1.0"
        sources."is-symbol-1.0.1"
        sources."is-typedarray-1.0.0"
        sources."is-url-1.2.2"
        sources."is-utf8-0.2.1"
        sources."is-windows-1.0.2"
        sources."is-wsl-1.1.0"
        sources."isarray-2.0.1"
        sources."isexe-2.0.0"
        sources."isnumeric-0.2.0"
        sources."isobject-2.1.0"
        sources."isomorphic-fetch-2.2.1"
        sources."isstream-0.1.2"
        sources."js-base64-2.4.3"
        sources."js-polyfills-0.1.42"
        sources."js-string-escape-1.0.1"
        sources."js-tokens-3.0.2"
        sources."js-yaml-3.11.0"
        sources."jsbn-0.1.1"
        (sources."jsdom-11.6.2" // {
          dependencies = [
            sources."punycode-2.1.0"
            sources."ws-4.1.0"
          ];
        })
        sources."jsesc-1.3.0"
        sources."json-loader-0.5.7"
        sources."json-parse-better-errors-1.0.1"
        sources."json-schema-0.2.3"
        sources."json-schema-traverse-0.3.1"
        sources."json-stringify-safe-5.0.1"
        sources."json3-3.3.2"
        sources."json5-0.5.1"
        sources."jsprim-1.4.1"
        sources."killable-1.0.0"
        sources."kind-of-3.2.2"
        sources."last-line-stream-1.0.0"
        sources."latest-version-3.1.0"
        sources."lazy-cache-1.0.4"
        sources."lcid-1.0.0"
        sources."left-pad-1.2.0"
        sources."levn-0.3.0"
        sources."load-json-file-2.0.0"
        sources."loader-runner-2.3.0"
        sources."loader-utils-1.1.0"
        sources."locate-path-2.0.0"
        sources."lodash-4.17.5"
        sources."lodash._reinterpolate-3.0.0"
        sources."lodash.camelcase-4.3.0"
        sources."lodash.clonedeep-4.5.0"
        sources."lodash.clonedeepwith-4.5.0"
        sources."lodash.debounce-4.0.8"
        sources."lodash.difference-4.5.0"
        sources."lodash.flatten-4.4.0"
        sources."lodash.flattendeep-4.4.0"
        sources."lodash.isequal-4.5.0"
        sources."lodash.memoize-4.1.2"
        sources."lodash.merge-4.6.1"
        sources."lodash.sortby-4.7.0"
        sources."lodash.template-4.4.0"
        sources."lodash.templatesettings-4.1.0"
        sources."lodash.uniq-4.5.0"
        sources."loglevel-1.6.1"
        sources."longest-1.0.1"
        sources."loose-envify-1.3.1"
        sources."lost-8.2.1"
        sources."loud-rejection-1.6.0"
        sources."lower-case-1.1.4"
        sources."lowercase-keys-1.0.0"
        sources."lru-cache-4.1.2"
        sources."macaddress-0.2.8"
        sources."make-dir-1.2.0"
        sources."map-cache-0.2.2"
        sources."map-obj-1.0.1"
        sources."map-visit-1.0.0"
        sources."matcher-1.1.0"
        sources."math-expression-evaluator-1.2.17"
        sources."md5-hex-2.0.0"
        sources."md5-o-matic-0.1.1"
        sources."md5.js-1.3.4"
        sources."media-typer-0.3.0"
        sources."mem-1.1.0"
        sources."memory-fs-0.4.1"
        (sources."meow-3.7.0" // {
          dependencies = [
            sources."indent-string-2.1.0"
            sources."pify-2.3.0"
          ];
        })
        sources."merge-descriptors-1.0.1"
        sources."methods-1.1.2"
        sources."micromatch-2.3.11"
        sources."miller-rabin-4.0.1"
        sources."mime-1.6.0"
        sources."mime-db-1.33.0"
        sources."mime-types-2.1.18"
        sources."mimic-fn-1.2.0"
        sources."minimalistic-assert-1.0.0"
        sources."minimalistic-crypto-utils-1.0.1"
        sources."minimatch-3.0.4"
        sources."minimist-0.0.8"
        sources."mississippi-2.0.0"
        sources."mixin-deep-1.3.1"
        sources."mkdirp-0.5.1"
        sources."module-alias-2.0.6"
        sources."moment-2.21.0"
        sources."move-concurrently-1.0.1"
        sources."ms-2.0.0"
        sources."multicast-dns-6.2.3"
        sources."multicast-dns-service-types-1.1.0"
        sources."multimatch-2.1.0"
        sources."nan-2.9.2"
        sources."nanomatch-1.2.9"
        sources."ncname-1.0.0"
        sources."negotiator-0.6.1"
        sources."neo-async-2.5.0"
        sources."no-case-2.3.2"
        sources."node-fetch-1.7.3"
        sources."node-forge-0.7.1"
        sources."node-libs-browser-2.1.0"
        sources."normalize-package-data-2.4.0"
        sources."normalize-path-2.1.1"
        sources."normalize-range-0.1.2"
        sources."normalize-url-1.9.1"
        sources."npm-run-path-2.0.2"
        sources."nth-check-1.0.1"
        sources."num2fraction-1.2.2"
        sources."number-is-nan-1.0.1"
        sources."nwmatcher-1.4.3"
        sources."oauth-sign-0.8.2"
        sources."object-assign-4.1.1"
        sources."object-component-0.0.3"
        sources."object-copy-0.1.0"
        sources."object-keys-1.0.11"
        sources."object-visit-1.0.1"
        sources."object.omit-2.0.1"
        sources."object.pick-1.3.0"
        sources."observable-to-promise-0.5.0"
        sources."obuf-1.1.1"
        sources."on-finished-2.3.0"
        sources."on-headers-1.0.1"
        sources."once-1.4.0"
        sources."onecolor-3.0.5"
        sources."onetime-2.0.1"
        sources."opn-5.2.0"
        sources."option-chain-1.0.0"
        sources."optionator-0.8.2"
        sources."original-1.0.0"
        sources."os-browserify-0.3.0"
        sources."os-homedir-1.0.2"
        sources."os-locale-2.1.0"
        sources."os-tmpdir-1.0.2"
        sources."p-finally-1.0.0"
        sources."p-limit-1.2.0"
        sources."p-locate-2.0.0"
        sources."p-map-1.2.0"
        sources."p-try-1.0.0"
        sources."package-hash-2.0.0"
        sources."package-json-4.0.1"
        sources."pako-1.0.6"
        sources."parallel-transform-1.1.0"
        sources."param-case-2.1.1"
        sources."parse-asn1-5.1.0"
        sources."parse-glob-3.0.4"
        sources."parse-json-2.2.0"
        sources."parse-ms-1.0.1"
        sources."parse5-4.0.0"
        sources."parseqs-0.0.5"
        sources."parseuri-0.0.5"
        sources."parseurl-1.3.2"
        sources."pascalcase-0.1.1"
        sources."path-browserify-0.0.0"
        sources."path-dirname-1.0.2"
        sources."path-exists-3.0.0"
        sources."path-is-absolute-1.0.1"
        sources."path-is-inside-1.0.2"
        sources."path-key-2.0.1"
        sources."path-parse-1.0.5"
        sources."path-to-regexp-0.1.7"
        sources."path-type-3.0.0"
        sources."pbkdf2-3.0.14"
        sources."performance-now-2.1.0"
        sources."pify-3.0.0"
        sources."pinkie-1.0.0"
        sources."pinkie-promise-1.0.0"
        sources."pixrem-4.0.1"
        sources."pkg-conf-2.1.0"
        sources."pkg-dir-2.0.0"
        sources."pleeease-filters-4.0.0"
        sources."plur-2.1.2"
        sources."pn-1.1.0"
        (sources."portfinder-1.0.13" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."posix-character-classes-0.1.1"
        sources."postcss-6.0.19"
        sources."postcss-apply-0.8.0"
        sources."postcss-attribute-case-insensitive-2.0.0"
        (sources."postcss-button-0.3.7" // {
          dependencies = [
            sources."ansi-regex-3.0.0"
            sources."camelcase-4.1.0"
            sources."date-time-0.1.1"
            sources."debug-3.1.0"
            sources."detect-indent-5.0.0"
            sources."esprima-4.0.0"
            sources."find-up-1.1.2"
            (sources."globby-6.1.0" // {
              dependencies = [
                sources."pify-2.3.0"
              ];
            })
            sources."has-flag-2.0.0"
            sources."is-extglob-1.0.0"
            sources."is-glob-2.0.1"
            sources."isarray-1.0.0"
            sources."load-json-file-1.1.0"
            sources."minimist-1.2.0"
            sources."parse-json-4.0.0"
            sources."parse-ms-0.1.2"
            sources."path-exists-2.1.0"
            sources."path-type-2.0.0"
            sources."pify-2.3.0"
            sources."pinkie-2.0.4"
            sources."pinkie-promise-2.0.1"
            sources."read-pkg-1.1.0"
            sources."read-pkg-up-1.0.1"
            sources."sort-keys-2.0.0"
            sources."source-map-0.5.7"
            sources."strip-ansi-4.0.0"
            sources."strip-bom-2.0.0"
            sources."supports-color-4.5.0"
            sources."symbol-observable-1.2.0"
            sources."write-file-atomic-1.3.4"
          ];
        })
        sources."postcss-calc-5.3.1"
        (sources."postcss-color-function-4.0.1" // {
          dependencies = [
            sources."balanced-match-0.1.0"
            sources."debug-3.1.0"
          ];
        })
        sources."postcss-color-gray-4.1.0"
        (sources."postcss-color-hex-alpha-3.0.0" // {
          dependencies = [
            sources."color-1.0.3"
          ];
        })
        sources."postcss-color-hsl-2.0.0"
        (sources."postcss-color-hwb-3.0.0" // {
          dependencies = [
            sources."color-1.0.3"
          ];
        })
        sources."postcss-color-rebeccapurple-3.0.0"
        sources."postcss-color-rgb-2.0.0"
        sources."postcss-color-rgba-fallback-3.0.0"
        sources."postcss-colormin-2.2.2"
        sources."postcss-convert-values-2.6.1"
        (sources."postcss-css-reset-1.0.2" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."has-flag-1.0.0"
            sources."postcss-5.2.18"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        (sources."postcss-cssnext-3.1.0" // {
          dependencies = [
            sources."balanced-match-0.4.2"
            sources."caniuse-api-2.0.0"
            sources."color-2.0.1"
            sources."color-string-1.5.2"
            sources."is-arrayish-0.3.1"
            sources."postcss-calc-6.0.1"
            sources."reduce-css-calc-2.1.4"
          ];
        })
        sources."postcss-custom-media-6.0.0"
        sources."postcss-custom-properties-6.3.1"
        sources."postcss-custom-selectors-4.0.1"
        (sources."postcss-discard-comments-2.0.4" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."has-flag-1.0.0"
            sources."postcss-5.2.18"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        sources."postcss-discard-duplicates-2.1.0"
        sources."postcss-discard-empty-2.1.0"
        sources."postcss-discard-overridden-0.1.1"
        sources."postcss-discard-unused-2.2.3"
        (sources."postcss-extend-1.0.5" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."has-flag-1.0.0"
            sources."postcss-5.2.18"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        sources."postcss-filter-plugins-2.0.2"
        (sources."postcss-flexbox-1.0.3" // {
          dependencies = [
            sources."ava-0.20.0"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."ansi-styles-2.2.1"
                sources."supports-color-2.0.0"
              ];
            })
            sources."concordance-2.0.0"
            sources."find-cache-dir-0.1.1"
            sources."find-up-1.1.2"
            sources."globby-6.1.0"
            sources."has-flag-2.0.0"
            sources."matcher-0.1.2"
            sources."ms-1.0.0"
            sources."option-chain-0.1.1"
            sources."path-exists-2.1.0"
            sources."pify-2.3.0"
            sources."pinkie-2.0.4"
            sources."pinkie-promise-2.0.1"
            sources."pkg-dir-1.0.0"
            sources."postcss-5.2.18"
            (sources."pretty-ms-2.1.0" // {
              dependencies = [
                sources."plur-1.0.0"
              ];
            })
            sources."resolve-cwd-1.0.0"
            sources."resolve-from-2.0.0"
            sources."source-map-0.5.7"
            (sources."supports-color-3.2.3" // {
              dependencies = [
                sources."has-flag-1.0.0"
              ];
            })
          ];
        })
        sources."postcss-font-family-system-ui-3.0.0"
        sources."postcss-font-variant-3.0.0"
        sources."postcss-image-set-polyfill-0.3.5"
        (sources."postcss-import-11.1.0" // {
          dependencies = [
            sources."pify-2.3.0"
          ];
        })
        sources."postcss-initial-2.0.0"
        (sources."postcss-inline-svg-3.0.0" // {
          dependencies = [
            sources."domhandler-2.4.1"
            sources."htmlparser2-3.9.2"
          ];
        })
        sources."postcss-load-config-1.2.0"
        sources."postcss-load-options-1.2.0"
        sources."postcss-load-plugins-2.3.0"
        (sources."postcss-loader-2.1.1" // {
          dependencies = [
            sources."ajv-6.2.1"
            sources."minimist-1.2.0"
            sources."schema-utils-0.4.5"
          ];
        })
        sources."postcss-media-minmax-3.0.0"
        sources."postcss-media-query-parser-0.2.3"
        sources."postcss-merge-idents-2.1.7"
        sources."postcss-merge-longhand-2.0.2"
        sources."postcss-merge-rules-2.1.2"
        sources."postcss-message-helpers-2.0.0"
        sources."postcss-minify-font-values-1.0.5"
        sources."postcss-minify-gradients-1.0.5"
        sources."postcss-minify-params-1.2.2"
        sources."postcss-minify-selectors-2.1.1"
        (sources."postcss-modules-extract-imports-1.2.0" // {
          dependencies = [
            sources."ansi-styles-3.2.1"
            sources."chalk-2.3.2"
            sources."has-flag-3.0.0"
            sources."postcss-6.0.19"
            sources."source-map-0.6.1"
            sources."supports-color-5.3.0"
          ];
        })
        (sources."postcss-modules-local-by-default-1.2.0" // {
          dependencies = [
            sources."ansi-styles-3.2.1"
            sources."chalk-2.3.2"
            sources."has-flag-3.0.0"
            sources."postcss-6.0.19"
            sources."source-map-0.6.1"
            sources."supports-color-5.3.0"
          ];
        })
        (sources."postcss-modules-scope-1.1.0" // {
          dependencies = [
            sources."ansi-styles-3.2.1"
            sources."chalk-2.3.2"
            sources."has-flag-3.0.0"
            sources."postcss-6.0.19"
            sources."source-map-0.6.1"
            sources."supports-color-5.3.0"
          ];
        })
        (sources."postcss-modules-values-1.3.0" // {
          dependencies = [
            sources."ansi-styles-3.2.1"
            sources."chalk-2.3.2"
            sources."has-flag-3.0.0"
            sources."postcss-6.0.19"
            sources."source-map-0.6.1"
            sources."supports-color-5.3.0"
          ];
        })
        sources."postcss-nested-2.1.2"
        sources."postcss-nesting-4.2.1"
        sources."postcss-normalize-charset-1.1.1"
        sources."postcss-normalize-url-3.0.8"
        sources."postcss-ordered-values-2.2.3"
        sources."postcss-pseudo-class-any-link-4.0.0"
        sources."postcss-pseudoelements-5.0.0"
        sources."postcss-reduce-idents-2.4.0"
        sources."postcss-reduce-initial-1.0.1"
        sources."postcss-reduce-transforms-1.0.4"
        sources."postcss-replace-overflow-wrap-2.0.0"
        sources."postcss-selector-matches-3.0.1"
        sources."postcss-selector-not-3.0.1"
        sources."postcss-selector-parser-2.2.3"
        (sources."postcss-svgo-2.1.6" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            (sources."chalk-1.1.3" // {
              dependencies = [
                sources."supports-color-2.0.0"
              ];
            })
            sources."esprima-2.7.3"
            sources."has-flag-1.0.0"
            sources."js-yaml-3.7.0"
            sources."postcss-5.2.18"
            sources."source-map-0.5.7"
            sources."supports-color-3.2.3"
          ];
        })
        sources."postcss-unique-selectors-2.0.2"
        sources."postcss-value-parser-3.3.0"
        sources."postcss-zindex-2.2.0"
        sources."prelude-ls-1.1.2"
        sources."prepend-http-1.0.4"
        sources."preserve-0.2.0"
        sources."pretty-error-2.1.1"
        sources."pretty-ms-3.1.0"
        sources."private-0.1.8"
        sources."process-0.11.10"
        sources."process-nextick-args-2.0.0"
        sources."promise-7.3.1"
        sources."promise-inflight-1.0.1"
        sources."promise-retry-1.1.1"
        sources."prop-types-15.6.1"
        sources."proxy-addr-2.0.3"
        sources."prr-1.0.1"
        sources."pseudomap-1.0.2"
        sources."public-encrypt-4.0.0"
        sources."pump-2.0.1"
        sources."pumpify-1.4.0"
        sources."punycode-1.4.1"
        sources."purescript-psa-0.5.1"
        (sources."purs-loader-3.1.2" // {
          dependencies = [
            sources."ansi-styles-2.2.1"
            sources."chalk-1.1.3"
            sources."cross-spawn-3.0.1"
            sources."glob-6.0.4"
            sources."globby-4.1.0"
            sources."pify-2.3.0"
            sources."pinkie-2.0.4"
            sources."pinkie-promise-2.0.1"
            sources."supports-color-2.0.0"
          ];
        })
        sources."q-1.5.1"
        sources."qs-6.5.1"
        sources."query-string-4.3.4"
        sources."querystring-0.2.0"
        sources."querystring-es3-0.2.1"
        sources."querystringify-0.0.4"
        (sources."randomatic-1.1.7" // {
          dependencies = [
            (sources."is-number-3.0.0" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
          ];
        })
        sources."randombytes-2.0.6"
        sources."randomfill-1.0.4"
        sources."range-parser-1.2.0"
        sources."raw-body-2.3.2"
        sources."rc-1.2.5"
        sources."react-16.2.0"
        sources."react-dom-16.2.0"
        sources."read-cache-1.0.0"
        sources."read-pkg-2.0.0"
        sources."read-pkg-up-2.0.0"
        sources."readable-stream-2.3.5"
        sources."readdirp-2.1.0"
        sources."redent-1.0.0"
        sources."reduce-css-calc-1.3.0"
        sources."reduce-function-call-1.0.2"
        sources."regenerate-1.3.3"
        sources."regenerator-runtime-0.10.5"
        sources."regenerator-transform-0.10.1"
        sources."regex-cache-0.4.4"
        sources."regex-not-1.0.2"
        sources."regexpu-core-2.0.0"
        sources."registry-auth-token-3.3.2"
        sources."registry-url-3.1.0"
        sources."regjsgen-0.2.0"
        sources."regjsparser-0.1.5"
        sources."relateurl-0.2.7"
        sources."release-zalgo-1.0.0"
        sources."remove-trailing-separator-1.1.0"
        (sources."renderkid-2.0.1" // {
          dependencies = [
            sources."utila-0.3.3"
          ];
        })
        sources."repeat-element-1.1.2"
        sources."repeat-string-1.6.1"
        sources."repeating-2.0.1"
        (sources."replace-in-file-3.2.0" // {
          dependencies = [
            sources."ansi-regex-3.0.0"
            sources."camelcase-4.1.0"
            sources."is-fullwidth-code-point-1.0.0"
            sources."strip-ansi-4.0.0"
            sources."y18n-3.2.1"
          ];
        })
        sources."request-2.83.0"
        sources."request-promise-core-1.1.1"
        sources."request-promise-native-1.0.5"
        sources."require-directory-2.1.1"
        sources."require-from-string-1.2.1"
        sources."require-main-filename-1.0.1"
        sources."require-precompiled-0.1.0"
        sources."requires-port-1.0.0"
        sources."resolve-1.5.0"
        sources."resolve-cwd-2.0.0"
        sources."resolve-from-3.0.0"
        sources."resolve-url-0.2.1"
        sources."restore-cursor-2.0.0"
        sources."ret-0.1.15"
        sources."retry-0.10.1"
        sources."rgb-0.1.0"
        sources."rgb-hex-2.1.0"
        sources."right-align-0.1.3"
        sources."rimraf-2.6.2"
        sources."ripemd160-2.0.1"
        sources."run-queue-1.0.3"
        sources."safe-buffer-5.1.1"
        sources."safe-regex-1.1.0"
        sources."sax-1.2.4"
        sources."schema-utils-0.3.0"
        sources."select-hose-2.0.0"
        sources."selfsigned-1.10.2"
        sources."semver-5.5.0"
        sources."semver-diff-2.1.0"
        sources."send-0.16.1"
        sources."serialize-javascript-1.4.0"
        (sources."serve-index-1.9.1" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."serve-static-1.13.1"
        sources."set-blocking-2.0.0"
        sources."set-getter-0.1.0"
        sources."set-immediate-shim-1.0.1"
        sources."set-value-2.0.0"
        sources."setimmediate-1.0.5"
        sources."setprototypeof-1.1.0"
        sources."sha.js-2.4.10"
        sources."shebang-command-1.2.0"
        sources."shebang-regex-1.0.0"
        sources."signal-exit-3.0.2"
        sources."simple-swizzle-0.2.2"
        sources."slash-1.0.0"
        sources."slice-ansi-1.0.0"
        sources."slide-1.1.6"
        (sources."snapdragon-0.8.1" // {
          dependencies = [
            (sources."define-property-0.2.5" // {
              dependencies = [
                sources."kind-of-5.1.0"
              ];
            })
            sources."extend-shallow-2.0.1"
            sources."kind-of-4.0.0"
          ];
        })
        (sources."snapdragon-node-2.1.1" // {
          dependencies = [
            sources."kind-of-3.2.2"
          ];
        })
        sources."snapdragon-util-3.0.1"
        sources."sntp-2.1.0"
        sources."socket.io-client-2.0.4"
        (sources."socket.io-parser-3.1.3" // {
          dependencies = [
            sources."debug-3.1.0"
          ];
        })
        sources."sockjs-0.3.19"
        (sources."sockjs-client-1.1.4" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."sort-keys-1.1.2"
        sources."source-list-map-2.0.0"
        sources."source-map-0.6.1"
        (sources."source-map-loader-0.2.3" // {
          dependencies = [
            sources."loader-utils-0.2.17"
          ];
        })
        sources."source-map-resolve-0.5.1"
        sources."source-map-support-0.4.18"
        sources."source-map-url-0.4.0"
        sources."spdx-correct-3.0.0"
        sources."spdx-exceptions-2.1.0"
        sources."spdx-expression-parse-3.0.0"
        sources."spdx-license-ids-3.0.0"
        (sources."spdy-3.4.7" // {
          dependencies = [
            sources."debug-2.6.9"
          ];
        })
        sources."spdy-transport-2.0.20"
        (sources."split-string-3.1.0" // {
          dependencies = [
            sources."extend-shallow-3.0.2"
            sources."is-extendable-1.0.1"
          ];
        })
        sources."sprintf-js-1.0.3"
        sources."sshpk-1.13.1"
        sources."ssri-5.2.4"
        sources."stack-utils-1.0.1"
        sources."static-extend-0.1.2"
        sources."statuses-1.3.1"
        sources."stealthy-require-1.1.1"
        sources."stream-browserify-2.0.1"
        sources."stream-each-1.2.2"
        sources."stream-http-2.8.0"
        sources."stream-shift-1.0.0"
        sources."strict-uri-encode-1.1.0"
        sources."string-width-2.1.1"
        sources."string_decoder-1.0.3"
        sources."stringstream-0.0.5"
        sources."strip-ansi-3.0.1"
        sources."strip-bom-3.0.0"
        sources."strip-bom-buf-1.0.0"
        sources."strip-eof-1.0.0"
        sources."strip-indent-1.0.1"
        sources."strip-json-comments-2.0.1"
        sources."style-loader-0.19.1"
        sources."supports-color-5.3.0"
        sources."svgo-0.7.2"
        sources."symbol-observable-0.2.4"
        sources."symbol-tree-3.2.2"
        sources."tapable-0.2.8"
        sources."term-size-1.2.0"
        sources."text-table-0.2.0"
        sources."through2-2.0.3"
        sources."thunky-1.0.2"
        (sources."time-require-0.1.2" // {
          dependencies = [
            sources."ansi-styles-1.0.0"
            sources."chalk-0.4.0"
            sources."pretty-ms-0.2.2"
            sources."strip-ansi-0.1.1"
          ];
        })
        sources."time-stamp-2.0.0"
        sources."time-zone-1.0.0"
        sources."timed-out-4.0.1"
        sources."timers-browserify-2.0.6"
        sources."to-array-0.1.4"
        sources."to-arraybuffer-1.0.1"
        sources."to-fast-properties-1.0.3"
        sources."to-object-path-0.3.0"
        sources."to-regex-3.0.2"
        sources."to-regex-range-2.1.1"
        sources."toposort-1.0.6"
        sources."tough-cookie-2.3.4"
        sources."tr46-1.0.1"
        sources."trim-newlines-1.0.0"
        sources."trim-off-newlines-1.0.1"
        sources."trim-right-1.0.1"
        sources."tty-browserify-0.0.0"
        sources."tunnel-agent-0.6.0"
        sources."tweetnacl-0.14.5"
        sources."type-check-0.3.2"
        sources."type-is-1.6.16"
        sources."typedarray-0.0.6"
        sources."ua-parser-js-0.7.17"
        sources."uglify-js-2.8.29"
        sources."uglify-to-browserify-1.0.2"
        (sources."uglifyjs-webpack-plugin-0.4.6" // {
          dependencies = [
            sources."yargs-3.10.0"
          ];
        })
        sources."uid2-0.0.3"
        sources."ultron-1.1.1"
        (sources."union-value-1.0.0" // {
          dependencies = [
            sources."set-value-0.4.3"
          ];
        })
        sources."uniq-1.0.1"
        sources."uniqid-4.1.1"
        sources."uniqs-2.0.0"
        sources."unique-filename-1.1.0"
        sources."unique-slug-2.0.0"
        sources."unique-string-1.0.0"
        sources."unique-temp-dir-1.0.0"
        sources."units-css-0.4.0"
        sources."unpipe-1.0.0"
        (sources."unset-value-1.0.0" // {
          dependencies = [
            (sources."has-value-0.3.1" // {
              dependencies = [
                sources."isobject-2.1.0"
              ];
            })
          ];
        })
        sources."unzip-response-2.0.1"
        sources."upath-1.0.4"
        sources."update-notifier-2.3.0"
        sources."upper-case-1.1.3"
        sources."urix-0.1.0"
        (sources."url-0.11.0" // {
          dependencies = [
            sources."punycode-1.3.2"
          ];
        })
        sources."url-loader-0.6.2"
        sources."url-parse-1.2.0"
        sources."url-parse-lax-1.0.0"
        sources."use-2.0.2"
        sources."util-0.10.3"
        sources."util-deprecate-1.0.2"
        sources."utila-0.4.0"
        sources."utils-merge-1.0.1"
        sources."uuid-3.2.1"
        sources."validate-npm-package-license-3.0.3"
        sources."vary-1.1.2"
        sources."vendors-1.0.1"
        sources."verror-1.10.0"
        sources."viewport-dimensions-0.2.0"
        sources."vm-browserify-0.0.4"
        sources."w3c-hr-time-1.0.1"
        (sources."watchpack-1.5.0" // {
          dependencies = [
            sources."is-accessor-descriptor-1.0.0"
            sources."is-data-descriptor-1.0.0"
            sources."is-descriptor-1.0.2"
            sources."is-extendable-0.1.1"
            (sources."is-number-3.0.0" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
          ];
        })
        sources."waypoints-4.0.1"
        sources."wbuf-1.7.2"
        sources."webidl-conversions-4.0.2"
        (sources."webpack-3.11.0" // {
          dependencies = [
            sources."ajv-6.2.1"
            (sources."anymatch-2.0.0" // {
              dependencies = [
                sources."is-extendable-0.1.1"
              ];
            })
            sources."arr-diff-4.0.0"
            sources."array-unique-0.3.2"
            (sources."braces-2.3.1" // {
              dependencies = [
                sources."is-accessor-descriptor-1.0.0"
                sources."is-data-descriptor-1.0.0"
                sources."is-descriptor-1.0.2"
                sources."is-extendable-0.1.1"
              ];
            })
            sources."camelcase-1.2.1"
            (sources."chokidar-2.0.2" // {
              dependencies = [
                (sources."is-accessor-descriptor-0.1.6" // {
                  dependencies = [
                    sources."kind-of-3.2.2"
                  ];
                })
                (sources."is-data-descriptor-0.1.4" // {
                  dependencies = [
                    sources."kind-of-3.2.2"
                  ];
                })
                sources."is-descriptor-0.1.6"
                sources."is-extendable-1.0.1"
              ];
            })
            sources."cliui-2.1.0"
            sources."define-property-1.0.0"
            (sources."expand-brackets-2.1.4" // {
              dependencies = [
                sources."define-property-0.2.5"
              ];
            })
            sources."extend-shallow-2.0.1"
            (sources."extglob-2.0.4" // {
              dependencies = [
                sources."define-property-1.0.0"
                sources."extend-shallow-2.0.1"
                sources."kind-of-5.1.0"
              ];
            })
            sources."fill-range-4.0.0"
            (sources."glob-parent-3.1.0" // {
              dependencies = [
                sources."is-glob-3.1.0"
              ];
            })
            sources."has-flag-2.0.0"
            sources."has-values-0.1.4"
            sources."hash-base-2.0.2"
            sources."inherits-2.0.1"
            (sources."is-accessor-descriptor-0.1.6" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
            (sources."is-data-descriptor-0.1.4" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
            sources."is-descriptor-0.1.6"
            sources."is-extendable-1.0.1"
            sources."is-fullwidth-code-point-1.0.0"
            sources."is-number-4.0.0"
            sources."isarray-1.0.0"
            sources."isobject-3.0.1"
            sources."kind-of-6.0.2"
            sources."lazy-cache-2.0.2"
            sources."micromatch-3.1.9"
            sources."source-map-0.5.7"
            sources."supports-color-4.5.0"
            sources."uglify-js-2.8.29"
            sources."wordwrap-0.0.2"
            sources."y18n-3.2.1"
            (sources."yargs-8.0.2" // {
              dependencies = [
                sources."camelcase-4.1.0"
                (sources."cliui-3.2.0" // {
                  dependencies = [
                    sources."string-width-1.0.2"
                  ];
                })
              ];
            })
            sources."yargs-parser-7.0.0"
          ];
        })
        (sources."webpack-dev-middleware-1.12.2" // {
          dependencies = [
            sources."mime-1.6.0"
          ];
        })
        (sources."webpack-dev-server-2.11.2" // {
          dependencies = [
            sources."anymatch-2.0.0"
            sources."arr-diff-4.0.0"
            sources."array-flatten-1.1.1"
            sources."array-unique-0.3.2"
            sources."async-1.5.2"
            sources."braces-2.3.1"
            sources."camelcase-3.0.0"
            (sources."chokidar-2.0.2" // {
              dependencies = [
                sources."debug-2.6.9"
                sources."is-accessor-descriptor-1.0.0"
                sources."is-data-descriptor-1.0.0"
                sources."is-descriptor-1.0.2"
              ];
            })
            sources."cliui-3.2.0"
            sources."debug-3.1.0"
            sources."define-property-1.0.0"
            (sources."expand-brackets-2.1.4" // {
              dependencies = [
                sources."define-property-0.2.5"
              ];
            })
            sources."extend-shallow-2.0.1"
            (sources."extglob-2.0.4" // {
              dependencies = [
                sources."define-property-1.0.0"
                sources."extend-shallow-2.0.1"
                sources."kind-of-5.1.0"
              ];
            })
            sources."faye-websocket-0.11.1"
            (sources."fill-range-4.0.0" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
            sources."find-up-1.1.2"
            (sources."glob-parent-3.1.0" // {
              dependencies = [
                sources."is-glob-3.1.0"
              ];
            })
            (sources."globby-6.1.0" // {
              dependencies = [
                sources."pify-2.3.0"
              ];
            })
            sources."import-local-1.0.0"
            (sources."is-accessor-descriptor-0.1.6" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
            (sources."is-data-descriptor-0.1.4" // {
              dependencies = [
                sources."kind-of-3.2.2"
              ];
            })
            sources."is-descriptor-0.1.6"
            sources."is-extglob-1.0.0"
            sources."is-fullwidth-code-point-1.0.0"
            sources."is-glob-3.1.0"
            sources."is-number-3.0.0"
            sources."isobject-3.0.1"
            sources."kind-of-6.0.2"
            sources."load-json-file-1.1.0"
            sources."micromatch-3.1.9"
            sources."mime-1.4.1"
            sources."os-locale-1.4.0"
            sources."path-exists-2.1.0"
            sources."path-type-1.1.0"
            sources."pify-2.3.0"
            sources."pinkie-2.0.4"
            sources."pinkie-promise-2.0.1"
            sources."querystringify-1.0.0"
            sources."read-pkg-1.1.0"
            sources."read-pkg-up-1.0.1"
            sources."string-width-1.0.2"
            sources."strip-bom-2.0.0"
            sources."which-module-1.0.0"
            sources."y18n-3.2.1"
            sources."yargs-6.6.0"
            sources."yargs-parser-4.2.1"
          ];
        })
        sources."webpack-sources-1.1.0"
        sources."websocket-driver-0.7.0"
        sources."websocket-extensions-0.1.3"
        sources."well-known-symbols-1.0.0"
        sources."whatwg-encoding-1.0.3"
        sources."whatwg-fetch-2.0.3"
        sources."whatwg-url-6.4.0"
        sources."whet.extend-0.9.9"
        sources."which-1.3.0"
        sources."which-module-2.0.0"
        sources."widest-line-2.0.0"
        sources."window-size-0.1.0"
        sources."wordwrap-1.0.0"
        (sources."wrap-ansi-2.1.0" // {
          dependencies = [
            sources."string-width-1.0.2"
            sources."strip-ansi-3.0.1"
          ];
        })
        sources."wrappy-1.0.2"
        sources."write-file-atomic-2.3.0"
        sources."write-json-file-2.3.0"
        sources."write-pkg-3.1.0"
        sources."ws-3.3.3"
        sources."xdg-basedir-3.0.0"
        sources."xml-char-classes-1.0.0"
        sources."xml-name-validator-3.0.0"
        sources."xmlhttprequest-ssl-1.5.5"
        sources."xtend-4.0.1"
        sources."y18n-4.0.0"
        sources."yallist-2.1.2"
        (sources."yargs-11.1.0" // {
          dependencies = [
            sources."ansi-regex-2.1.1"
          ];
        })
        sources."yargs-parser-9.0.2"
        sources."yeast-0.1.2"
      ];
      buildInputs = globalBuildInputs;
      meta = {
        description = "Frontend of 'cardano-sl' explorer";
        homepage = "https://github.com/input-output-hk/cardano-sl/tree/master/explorer#readme";
        license = "MIT";
      };
      production = false;
      bypassCache = false;
    };
    in {
      tarball = nodeEnv.buildNodeSourceDist args;
      package = nodeEnv.buildNodePackage args;
      shell = nodeEnv.buildNodeShell args;
    }
