---
title: Two Roots
tangles:
- name: main
  path: main.c
  language: cee
- name: func
  path: func.c
  language: cee
---

``` {#main}
int main(void) {
    <<hello>>
}
```

``` {#func}
int func(void) {
    if (0 < 2) {
        <<hello>>
    }
}
```

``` {#hello}
printf("Hello, %s!\n", "World");
```

``` {#hello}
return 0;
```
