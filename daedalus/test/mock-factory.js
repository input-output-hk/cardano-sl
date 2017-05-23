
export const mockSuccessResponse = (response={}) => ({ Right: response });
export const mockErrorResponse = (errMsg='Any error') => ({ Left: errMsg });

export const mockWallet = (address='abc', props={}) => ({...
  { cwId: `${address}`,
    cwAmount: {
      getCoin: 33333
    },
    cwMeta: {
      cwType: 'CWTPersonal',
      cwCurrency: 'ADA',
      "cwName": ''
    }
  }, ...props});

export const mockTransaction = (id="123", props={}) => ({...
  { ctId: `${id}`,
    ctAmount: {
      getCoin: 1000
    },
    ctType:{
      tag: 'CTOut',
      contents: {
        ctmCurrency: 'ADA',
        ctmTitle: '',
        ctmDescription: '',
        ctmDate: 1.483461872037636e9
      }
    }
  }, ...props});
