{ stdenv
, runCommand
, python3Packages
, writeScriptBin
}:

let


  records = python3Packages.buildPythonPackage rec {
    version = "0.5.2";
    pname = "records";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "11inmhiggw3skab9g1cp9bdpi7kx0ayrbcdvjd275fzgx0svm313";
    };
    propagatedBuildInputs = with python3Packages; [
      psycopg2
      sqlalchemy
      docopt
      tablib
      unicodecsv
    ];
  };
  prometheus_flask_exporter = python3Packages.buildPythonPackage rec {
    version = "0.9.0";
    pname = "prometheus_flask_exporter";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "03l0k1k1vmshkv8z11s3i2bvbb8z6qsn6w3r2dzh50qh2sdx85k0";
    };
    propagatedBuildInputs = with python3Packages; [
      flask
      prometheus_client
    ];
  };
  explorer-python-api = python3Packages.buildPythonPackage {
    version = "3.0.3";
    pname = "explorer-python-api";
    src = ./.; # TODO: filterSrc
    propagatedBuildInputs = with python3Packages; [
      flask
      records
      prometheus_flask_exporter
      requests
      pytz
    ];
    doCheck = false;
  };

in runCommand "run-explorer-python-api" { buildInputs = [ explorer-python-api python3Packages.ipython ]; passthru = { inherit explorer-python-api; };} ''
  mkdir -p $out/bin
  cat << EOF > $out/bin/run-explorer-python-api
  #!${stdenv.shell}
  export PYTHONPATH="$PYTHONPATH"
  exec ${python3Packages.gunicorn}/bin/gunicorn --workers=1 --bind=0.0.0.0:7000 --timeout 300 explorer_python_api:app "\$@"
  EOF
  chmod +x $out/bin/run-explorer-python-api
''
