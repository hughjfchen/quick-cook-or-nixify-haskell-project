{ pkgs, ... }:
{
  config.project.name = "{{name}}";
  config.services = {

    {{name}}-service = {
      image.enableRecommendedContents = true;
      service.useHostStore = true;
      service.command = [ "${pkgs.{{name}}.{{name}}-exe.outPath}/bin/{{name}}" ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.environment.ENV1= "${pkgs.{{name}}.{{name}}-exe.outPath}/share/doc";
      service.stop_signal = "SIGINT";
    };
  };
}
