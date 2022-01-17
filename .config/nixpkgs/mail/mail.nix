{ config, pkgs, ... }:

{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
    };
  };
  accounts.email.accounts = {
    bbo = {
      address = "naoki@bbo.cs.tsukuba.ac.jp";
      # gpg = {
      #   key = "F9119EC8FCC56192B5CF53A0BF4F64254BD8C8B5";
      #   signByDefault = true;
      # };
      imap = {
        host = "imap.gmail.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "INBOX" "*Gmail*" "![Gmail]/Spam" ];
      };
      msmtp.enable = true;
      notmuch.enable = true;
      primary = true;
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.bbo-mail-pw";
      smtp = {
        host = "smtp.gmail.com";
      };
      userName = "naoki@bbo.cs.tsukuba.ac.jp";
    };

    private = {
      address = "nok.skmt.snow@gmail.com";
      # gpg = {
      #   key = "F9119EC8FCC56192B5CF53A0BF4F64254BD8C8B5";
      #   signByDefault = true;
      # };
      imap = {
        host = "imap.gmail.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "INBOX" "*Gmail*" "![Gmail]/Spam" ];
      };
      msmtp.enable = true;
      notmuch.enable = true;
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.nok-mail-pw";
      smtp = {
        host = "smtp.gmail.com";
      };
      userName = "nok.skmt.snow@gmail.com";
    };

    univ = {
      address = "s1930160@s.tsukuba.ac.jp";
      # gpg = {
      #   key = "F9119EC8FCC56192B5CF53A0BF4F64254BD8C8B5";
      #   signByDefault = true;
      # };
      imap = {
        host = "outlook.office365.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "*" ];
      };
      msmtp.enable = true;
      notmuch.enable = true;
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.s19-mail-pw";
      smtp = {
        host = "smtp.office365.com";
      };
      userName = "s1930160@s.tsukuba.ac.jp";
    };
  };
}
