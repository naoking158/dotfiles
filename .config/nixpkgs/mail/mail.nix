{ config, pkgs, ... }:

{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    search.excludeTags = [ "del" "spam" ];
    new.tags = [ "inbox" ];
    hooks.preNew = "mbsync --all";
  };
  accounts.email.accounts = {
    bbo = {
      primary = true;
      address = "naoki@bbo.cs.tsukuba.ac.jp";
      userName = "naoki@bbo.cs.tsukuba.ac.jp";
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.bbo-mail-pw";

      notmuch.enable = true;
      msmtp.enable = true;
      smtp = {
        host = "smtp.gmail.com";
      };
      imap = {
        host = "imap.gmail.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "INBOX" "*Gmail*" "![Gmail]/Spam" ];
      };
    };
    
    private = {
      address = "nok.skmt.snow@gmail.com";
      userName = "nok.skmt.snow@gmail.com";
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.nok-mail-pw";

      notmuch.enable = true;
      msmtp.enable = true;
      smtp = {
        host = "smtp.gmail.com";
      };
      imap = {
        host = "imap.gmail.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "INBOX" "*Gmail*" "![Gmail]/Spam" ];
      };
    };

    univ = {
      address = "s1930160@s.tsukuba.ac.jp";
      userName = "s1930160@s.tsukuba.ac.jp";
      realName = "Naoki Sakamoto";
      passwordCommand = "cat ~/src/github.com/naoking158/envs/.s19-mail-pw";

      notmuch.enable = true;
      msmtp.enable = true;
      smtp = {
        host = "smtp.office365.com";
      };
      imap = {
        host = "outlook.office365.com";
        port = 993;
      };
      mbsync = {
        enable = true;
        create = "both"; # Automatically create missing mailboxes within the given mail store. ["none", "maildir", "both"]
        patterns = [ "*" ];
      };
    };
  };
}
