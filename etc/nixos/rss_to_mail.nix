{ config, pkgs, lib, ... }:

# Run rss_to_mail periodically for a list of users.
#
# Using instiancied units rather than users units to keep things declarative.
# Rss_to_mail will be run as the target users and data will be stored in home
# directories.
#
# Usage:
#   services.rss_to_mail = {
#     enable = true;
#     users = [ "foo" "bar" ];
#   };

let
  rss_to_mail = pkgs.rss_to_mail;

  conf = config.services.rss_to_mail;
in

{
  options.services.rss_to_mail = with lib; {
    enable = mkEnableOption "rss_to_mail";

    users = mkOption {
      type = types.nonEmptyListOf types.str;
      default = [];
      description = "Users for which to enable Rss_to_mail.";
    };
  };

  config = lib.mkIf conf.enable {
    # Instance name is user
    systemd.services."rss_to_mail@" = {
      description = "Rss_to_mail for user %i.";
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "~";
        User = "%i";
        Environment="CA_BUNDLE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        ExecStart = "${rss_to_mail}/bin/rss_to_mail";
      };
    };

    systemd.timers."rss_to_mail@" = {
      description = "Runs rss_to_mail every 10 minutes";
      timerConfig = {
        OnBootSec = "10m";
        OnUnitInactiveSec = "10m";
        Unit = "rss_to_mail@%i.service";
      };
    };

    # Instanciate timers
    systemd.targets.timers.wants = map (u: "rss_to_mail@${u}.timer") conf.users;
  };
}
