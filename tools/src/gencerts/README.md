# GenCerts

> Pure Haskell 'replacement' for OpenSSL to generate certificates for a TLS Private Key Infrastructure

```
Usage: cardano-x509-certificates --server-out-dir FILEPATH
                                 --clients-out-dir FILEPATH
                                 [--ca-out-dir FILEPATH]
                                 (-k|--configuration-key KEY)
                                 (-c|--configuration-file FILEPATH)

Available options:
  --server-out-dir        FILEPATH Output dir for Server certificate & private key
  --clients-out-dir       FILEPATH Output dir for Client certificate(s) & private key
  --ca-out-dir            FILEPATH Optional, output dir for the CA certificate & private key
  -k,--configuration-key  KEY      Configuration key within the config file 
  -c,--configuration-file FILEPATH Configuration file describing the PKI
  -h,--help                        Show this help text
```

## How to Use

Considering a configuration file as follows:

<details>
<summary><strong>configuration.yaml</strong></summary>

```yaml
dev: 
  tls: 
    ca:
      organization: Input Output HK
      commonName: Cardano SL Self-Signed Root CA
      expiryDays: 3650

    server:
      organization: Input Output HK
      commonName: Cardano SL Server Node
      expiryDays: 365
      altDNS:
        - "localhost"
        - "localhost.localdomain"
        - "127.0.0.1"
        - "::1"

    clients:
      - organization: Input Output HK
        commonName: Daedalus Wallet
        expiryDays: 365
```
</details>

One can generate a server and a client certificates via:

```bash
$ cardano-x509-certificates \
  --server-out-dir tls/server \
  --clients-out-dir tls/client \
  -k dev \
  -c ./configuration.yaml
```

which generates the following file structure:

```
.
|-- tls
|---- server
|------ server.crt
|------ server.key
|------ ca.crt
|---- client
|------ client.crt
|------ client.key
|------ ca.crt
```


## Human-Readable Output

Certificates & private keys are DER-encoded. `openssl` can be used to view their content if needed using the following commands:

<details>
<summary><code>openssl x509 -inform der -in cert.crt -text</code></summary>

```
Certificate:
    Data:
        Version: 3 (0x2)
        Serial Number: 2 (0x2)
    Signature Algorithm: sha256WithRSAEncryption
        Issuer: O=Input Output HK, CN=Cardano SL Self-Signed Root CA
        Validity
            Not Before: Apr  3 12:23:36 2018 GMT
            Not After : Apr  3 12:23:36 2019 GMT
        Subject: O=Input Output HK, CN=Cardano SL Server Node
        Subject Public Key Info:
            Public Key Algorithm: rsaEncryption
                Public-Key: (2048 bit)
                Modulus:
                    00:ba:a3:5d:44:a8:5c:79:0e:12:e3:44:ff:70:bb:
                    33:b3:0e:d3:26:97:23:fd:58:f7:9e:e8:cd:13:f6:
                    b0:85:d3:45:8d:d4:d1:47:b9:3f:1c:24:b0:d3:f1:
                    94:39:f2:fd:1a:0c:97:70:ae:72:17:ee:c9:6f:d8:
                    73:cd:8f:25:a5:e9:a2:71:c9:21:fc:3f:87:ec:b2:
                    61:df:20:cb:a8:62:f0:27:f5:69:e6:6c:e7:ea:cc:
                    4b:d0:18:5e:7a:aa:8c:3d:2f:74:63:c2:8b:5d:e8:
                    89:9f:89:0a:e0:5a:96:10:0b:a0:23:8c:27:27:8c:
                    6a:1d:ff:c9:4e:14:bf:1b:f1:bb:05:4b:0b:eb:46:
                    22:6c:da:e2:fd:ae:74:04:63:ff:ae:1a:42:cb:8e:
                    9e:ea:5a:b6:bd:b2:de:8c:20:14:89:36:0c:6c:3d:
                    16:dc:d5:5e:38:f6:7c:55:c3:2d:fc:61:f2:72:46:
                    5b:9d:3f:97:fa:91:f1:cb:0b:ed:64:30:4f:c8:c5:
                    5f:9a:85:03:4f:34:3a:1b:84:a0:33:fc:4c:14:06:
                    45:1a:ba:d1:8e:19:08:52:76:1b:05:46:dc:c0:05:
                    16:71:e9:b6:f5:80:2b:30:4f:e2:29:a5:77:48:ae:
                    a8:99:e4:d7:8e:27:d9:8d:9a:aa:fc:21:29:31:b8:
                    3f:31
                Exponent: 65537 (0x10001)
        X509v3 extensions:
            X509v3 Subject Alternative Name: 
                DNS:localhost, DNS:localhost.localdomain, DNS:127.0.0.1, DNS:::1
            X509v3 Key Usage: critical
                Digital Signature, Key Encipherment
            X509v3 Extended Key Usage: 
                TLS Web Server Authentication
            X509v3 Basic Constraints: 
                CA:FALSE
            X509v3 Subject Key Identifier: 
                84:5D:FE:C2
            X509v3 Authority Key Identifier: 
                keyid:6C:0B:A8:E8
    Signature Algorithm: sha256WithRSAEncryption
         34:77:d5:7a:31:ff:d2:29:10:3a:7b:88:b8:a5:79:98:e5:54:
         6f:ac:0b:4a:f1:cf:a2:b1:47:05:f8:6c:70:16:ca:62:32:14:
         40:df:b3:57:c1:92:12:2c:ab:ef:c1:1d:3d:20:d2:f8:e1:a0:
         d7:4c:87:bb:36:ed:8f:e5:c1:9d:55:3b:14:3a:bd:68:3a:05:
         cd:1a:00:f7:33:8f:6b:5c:71:ba:94:ad:00:84:85:47:aa:32:
         c4:fa:07:33:8d:0c:eb:2a:52:2c:92:1b:2f:08:6c:b9:3c:6a:
         3d:32:e3:d3:12:c4:e1:22:6d:a9:38:6a:81:8b:20:28:86:d8:
         11:ba:60:95:b4:9c:41:28:1c:d9:7c:04:8d:70:15:82:1a:63:
         66:51:8e:29:71:de:5e:64:cd:64:0e:52:7a:10:2f:e8:a7:d1:
         da:fa:ed:f8:b9:dc:0b:67:04:e4:20:84:57:dc:7f:57:9c:72:
         c9:ae:5b:90:d7:8c:33:d3:62:bb:dd:bd:7c:03:3b:51:56:d2:
         ab:f9:5c:65:3b:71:ec:d1:e0:7d:34:0d:fc:7b:0d:9f:84:12:
         e6:6b:85:15:7f:60:38:9c:fd:93:d1:e8:36:51:0e:ed:74:e5:
         f5:46:36:21:a4:d5:45:3c:21:92:08:6c:28:a8:e4:ac:7c:50:
         19:a8:65:7c
-----BEGIN CERTIFICATE-----
MIIDjjCCAnagAwIBAgIBAjANBgkqhkiG9w0BAQsFADBDMRgwFgYDVQQKDA9JbnB1
dCBPdXRwdXQgSEsxJzAlBgNVBAMMHkNhcmRhbm8gU0wgU2VsZi1TaWduZWQgUm9v
dCBDQTAeFw0xODA0MDMxMjIzMzZaFw0xOTA0MDMxMjIzMzZaMDsxGDAWBgNVBAoM
D0lucHV0IE91dHB1dCBISzEfMB0GA1UEAwwWQ2FyZGFubyBTTCBTZXJ2ZXIgTm9k
ZTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALqjXUSoXHkOEuNE/3C7
M7MO0yaXI/1Y957ozRP2sIXTRY3U0Ue5PxwksNPxlDny/RoMl3CuchfuyW/Yc82P
JaXponHJIfw/h+yyYd8gy6hi8Cf1aeZs5+rMS9AYXnqqjD0vdGPCi13oiZ+JCuBa
lhALoCOMJyeMah3/yU4UvxvxuwVLC+tGImza4v2udARj/64aQsuOnupatr2y3owg
FIk2DGw9FtzVXjj2fFXDLfxh8nJGW50/l/qR8csL7WQwT8jFX5qFA080OhuEoDP8
TBQGRRq60Y4ZCFJ2GwVG3MAFFnHptvWAKzBP4imld0iuqJnk144n2Y2aqvwhKTG4
PzECAwEAAaOBlDCBkTA7BgNVHREENDAygglsb2NhbGhvc3SCFWxvY2FsaG9zdC5s
b2NhbGRvbWFpboIJMTI3LjAuMC4xggM6OjEwDwYDVR0PAQH/BAUDAwegADATBgNV
HSUEDDAKBggrBgEFBQcDATAMBgNVHRMEBTADAQEAMA0GA1UdDgQGBASEXf7CMA8G
A1UdIwQIMAaABGwLqOgwDQYJKoZIhvcNAQELBQADggEBADR31Xox/9IpEDp7iLil
eZjlVG+sC0rxz6KxRwX4bHAWymIyFEDfs1fBkhIsq+/BHT0g0vjhoNdMh7s27Y/l
wZ1VOxQ6vWg6Bc0aAPczj2tccbqUrQCEhUeqMsT6BzONDOsqUiySGy8IbLk8aj0y
49MSxOEibak4aoGLICiG2BG6YJW0nEEoHNl8BI1wFYIaY2ZRjilx3l5kzWQOUnoQ
L+in0dr67fi53AtnBOQghFfcf1eccsmuW5DXjDPTYrvdvXwDO1FW0qv5XGU7cezR
4H00Dfx7DZ+EEuZrhRV/YDic/ZPR6DZRDu105fVGNiGk1UU8IZIIbCio5Kx8UBmo
ZXw=
-----END CERTIFICATE-----
```
</details>


<details>
<summary><code>openssl rsa -inform der -in key.key -text</code></summary>

```
Private-Key: (2048 bit)
modulus:
    00:ba:a3:5d:44:a8:5c:79:0e:12:e3:44:ff:70:bb:
    33:b3:0e:d3:26:97:23:fd:58:f7:9e:e8:cd:13:f6:
    b0:85:d3:45:8d:d4:d1:47:b9:3f:1c:24:b0:d3:f1:
    94:39:f2:fd:1a:0c:97:70:ae:72:17:ee:c9:6f:d8:
    73:cd:8f:25:a5:e9:a2:71:c9:21:fc:3f:87:ec:b2:
    61:df:20:cb:a8:62:f0:27:f5:69:e6:6c:e7:ea:cc:
    4b:d0:18:5e:7a:aa:8c:3d:2f:74:63:c2:8b:5d:e8:
    89:9f:89:0a:e0:5a:96:10:0b:a0:23:8c:27:27:8c:
    6a:1d:ff:c9:4e:14:bf:1b:f1:bb:05:4b:0b:eb:46:
    22:6c:da:e2:fd:ae:74:04:63:ff:ae:1a:42:cb:8e:
    9e:ea:5a:b6:bd:b2:de:8c:20:14:89:36:0c:6c:3d:
    16:dc:d5:5e:38:f6:7c:55:c3:2d:fc:61:f2:72:46:
    5b:9d:3f:97:fa:91:f1:cb:0b:ed:64:30:4f:c8:c5:
    5f:9a:85:03:4f:34:3a:1b:84:a0:33:fc:4c:14:06:
    45:1a:ba:d1:8e:19:08:52:76:1b:05:46:dc:c0:05:
    16:71:e9:b6:f5:80:2b:30:4f:e2:29:a5:77:48:ae:
    a8:99:e4:d7:8e:27:d9:8d:9a:aa:fc:21:29:31:b8:
    3f:31
publicExponent: 65537 (0x10001)
privateExponent:
    00:95:d4:78:17:9a:21:42:7d:6d:e4:4d:8b:d3:70:
    35:7c:d5:b5:e5:11:08:af:66:30:c3:bd:98:85:22:
    51:6b:dd:c3:c4:ac:c8:a5:8f:ad:7e:66:66:1e:7f:
    9d:c3:37:b2:c0:aa:c3:18:8e:b2:c1:4b:cd:22:a0:
    dd:b6:73:e8:81:5d:22:2f:be:8c:2d:f8:c2:a1:de:
    42:7b:e1:d4:1d:05:f0:4d:e3:d2:74:4d:91:91:09:
    19:03:a1:cc:97:25:3b:18:13:74:98:71:a1:78:6e:
    29:ad:8d:52:d1:f1:66:ee:d2:a6:68:63:27:3b:b4:
    99:ac:23:9f:f3:b2:ec:08:fd:92:5c:9e:8b:fb:5c:
    69:22:97:08:23:b1:f7:11:f8:52:99:d5:ab:8a:5c:
    c3:f6:7d:9c:5e:b6:3e:fd:c7:0c:ed:bb:00:bd:71:
    10:c6:7e:68:98:26:70:c8:17:4a:26:a6:a4:27:84:
    de:38:35:65:ff:90:da:a3:e7:58:11:32:8b:3a:f3:
    4b:18:c3:65:4a:e0:1a:81:c4:4a:68:6e:26:bf:e8:
    b6:00:6a:69:15:f8:3d:58:9e:6b:9b:8d:14:30:2c:
    54:45:7b:a5:12:53:e1:35:ee:e3:41:e6:ed:9e:ae:
    74:df:ba:7f:bb:76:67:a5:18:13:36:7c:7d:b2:bf:
    0b:d1
prime1:
    00:ec:8b:81:d9:70:c1:ae:50:72:6b:8d:0b:16:8d:
    e2:e8:fc:2e:73:e2:7b:68:65:28:46:2a:d3:bd:d2:
    06:5b:61:09:d1:8a:e9:31:c8:f8:20:90:c7:d0:86:
    49:5f:d1:55:d2:f1:3e:a3:bc:15:d8:76:a5:b4:7d:
    f5:b5:8b:c3:10:46:2d:a4:4b:14:c1:67:8b:55:ed:
    1b:a0:4e:a9:38:2d:87:e5:82:7e:78:a6:7c:1f:b5:
    56:fa:8a:67:65:d3:ff:6e:27:af:cc:4f:02:3e:bf:
    22:db:7b:9d:5d:10:3a:01:bc:9d:0a:fa:da:81:f3:
    3f:ec:e4:39:b1:8b:0a:e1:83
prime2:
    00:c9:fd:0f:a1:b8:9f:68:61:26:c3:cc:ec:2e:de:
    44:07:99:ba:1c:a5:7f:47:f1:8a:8e:2c:fc:fe:e1:
    59:10:1d:68:f1:7b:6d:ca:24:82:5f:15:15:76:32:
    45:57:4b:f4:b1:f4:53:4e:17:72:aa:87:7d:7b:9e:
    65:01:67:73:0b:c9:fd:b5:87:89:f5:58:4e:61:6b:
    0b:43:86:ec:d2:65:83:6c:b0:1d:78:a5:be:2c:b6:
    22:0b:96:19:40:8e:17:09:30:48:3a:a7:75:de:39:
    a0:97:d0:37:ca:29:0f:54:0e:9f:71:d9:02:50:d5:
    09:99:4b:9e:93:9f:9b:c2:3b
exponent1:
    26:d4:47:be:6f:d9:fb:49:4d:5f:06:4a:19:ab:b4:
    e1:d5:f3:b2:26:2d:67:d7:a3:22:d1:88:ec:91:fb:
    65:28:aa:b9:f0:f9:92:a8:90:c2:97:c1:95:23:56:
    0c:1a:8a:e2:13:a3:da:b0:d7:4d:2f:3c:c8:42:1d:
    01:6e:2d:d9:10:0c:11:fa:19:30:1e:55:2b:07:bf:
    0f:33:9a:67:94:61:c8:75:01:59:c0:ca:83:51:fb:
    33:29:61:0b:c0:1f:1f:ef:f7:d5:ab:ea:8c:6e:47:
    c4:8b:a8:2b:4c:ac:98:f8:63:37:18:32:a2:3e:51:
    f2:0b:0c:c5:6e:33:9d:0d
exponent2:
    0e:aa:e0:b2:b6:ac:64:b1:01:56:a0:8f:da:0b:6b:
    d7:3d:73:85:57:03:3a:1f:31:17:87:ee:cb:37:63:
    f7:46:9b:73:45:aa:40:4e:8c:65:09:2b:e8:cc:57:
    ba:b2:1a:4f:bd:d5:3a:cb:7b:19:25:03:98:b1:74:
    d6:38:43:e3:6c:44:8f:25:0b:80:94:6c:04:a8:f8:
    45:6c:0b:d0:6f:6c:ae:bc:cf:37:0e:9a:13:b7:1a:
    1b:cb:47:84:27:8d:46:ae:89:e0:30:7e:df:a7:41:
    a1:24:8e:98:90:88:bb:6a:e0:6b:2c:fd:fd:0e:ab:
    46:c5:75:fb:74:50:a4:37
coefficient:
    7a:88:d0:77:2f:27:fc:a4:d6:ee:30:62:27:86:aa:
    bf:dd:03:07:f1:98:87:c9:9a:58:65:5d:db:9f:d8:
    a3:cf:2e:86:15:c3:9f:ad:c3:40:2f:4e:ef:23:08:
    f5:9b:33:77:5d:d0:6f:aa:5b:76:37:13:83:62:21:
    ab:14:9b:d8:bd:0a:fc:ba:38:26:18:cc:f5:80:1f:
    e3:8d:9e:64:a7:8f:a5:61:c6:4f:9c:2a:06:28:37:
    24:43:cc:18:c2:f3:13:c2:7c:43:cc:d0:7d:01:fe:
    54:49:f7:f4:7f:bb:2e:36:17:50:f2:f4:da:06:55:
    2b:44:d9:ab:57:8b:a6:fb
writing RSA key
-----BEGIN RSA PRIVATE KEY-----
MIIEowIBAAKCAQEAuqNdRKhceQ4S40T/cLszsw7TJpcj/Vj3nujNE/awhdNFjdTR
R7k/HCSw0/GUOfL9GgyXcK5yF+7Jb9hzzY8lpemicckh/D+H7LJh3yDLqGLwJ/Vp
5mzn6sxL0BheeqqMPS90Y8KLXeiJn4kK4FqWEAugI4wnJ4xqHf/JThS/G/G7BUsL
60YibNri/a50BGP/rhpCy46e6lq2vbLejCAUiTYMbD0W3NVeOPZ8VcMt/GHyckZb
nT+X+pHxywvtZDBPyMVfmoUDTzQ6G4SgM/xMFAZFGrrRjhkIUnYbBUbcwAUWcem2
9YArME/iKaV3SK6omeTXjifZjZqq/CEpMbg/MQIDAQABAoIBAQCV1HgXmiFCfW3k
TYvTcDV81bXlEQivZjDDvZiFIlFr3cPErMilj61+ZmYef53DN7LAqsMYjrLBS80i
oN22c+iBXSIvvowt+MKh3kJ74dQdBfBN49J0TZGRCRkDocyXJTsYE3SYcaF4bimt
jVLR8Wbu0qZoYyc7tJmsI5/zsuwI/ZJcnov7XGkilwgjsfcR+FKZ1auKXMP2fZxe
tj79xwztuwC9cRDGfmiYJnDIF0ompqQnhN44NWX/kNqj51gRMos680sYw2VK4BqB
xEpobia/6LYAamkV+D1YnmubjRQwLFRFe6USU+E17uNB5u2ernTfun+7dmelGBM2
fH2yvwvRAoGBAOyLgdlwwa5QcmuNCxaN4uj8LnPie2hlKEYq073SBlthCdGK6THI
+CCQx9CGSV/RVdLxPqO8Fdh2pbR99bWLwxBGLaRLFMFni1XtG6BOqTgth+WCfnim
fB+1VvqKZ2XT/24nr8xPAj6/Itt7nV0QOgG8nQr62oHzP+zkObGLCuGDAoGBAMn9
D6G4n2hhJsPM7C7eRAeZuhylf0fxio4s/P7hWRAdaPF7bcokgl8VFXYyRVdL9LH0
U04XcqqHfXueZQFncwvJ/bWHifVYTmFrC0OG7NJlg2ywHXilviy2IguWGUCOFwkw
SDqndd45oJfQN8opD1QOn3HZAlDVCZlLnpOfm8I7AoGAJtRHvm/Z+0lNXwZKGau0
4dXzsiYtZ9ejItGI7JH7ZSiqufD5kqiQwpfBlSNWDBqK4hOj2rDXTS88yEIdAW4t
2RAMEfoZMB5VKwe/DzOaZ5RhyHUBWcDKg1H7MylhC8AfH+/31avqjG5HxIuoK0ys
mPhjNxgyoj5R8gsMxW4znQ0CgYAOquCytqxksQFWoI/aC2vXPXOFVwM6HzEXh+7L
N2P3RptzRapAToxlCSvozFe6shpPvdU6y3sZJQOYsXTWOEPjbESPJQuAlGwEqPhF
bAvQb2yuvM83DpoTtxoby0eEJ41GrongMH7fp0GhJI6YkIi7auBrLP39DqtGxXX7
dFCkNwKBgHqI0HcvJ/yk1u4wYieGqr/dAwfxmIfJmlhlXduf2KPPLoYVw5+tw0Av
Tu8jCPWbM3dd0G+qW3Y3E4NiIasUm9i9Cvy6OCYYzPWAH+ONnmSnj6Vhxk+cKgYo
NyRDzBjC8xPCfEPM0H0B/lRJ9/R/uy42F1Dy9NoGVStE2atXi6b7
-----END RSA PRIVATE KEY-----
```
</details>
