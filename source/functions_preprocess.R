#Function that assign a new label for type of attack
ClassLabelAttack = function(dataframe)
{
  newColumn = as.character(dataframe$V42)
  
  newColumn[newColumn == "apache2" | newColumn == "back" | newColumn == "land" | newColumn == "mailbomb"
            | newColumn == "neptune" | newColumn == "pod" | newColumn == "processtable" | newColumn == "smurf"
            | newColumn == "teardrop" | newColumn == "udpstorm"] = "DoS"
  
  newColumn[newColumn == "buffer_overflow" | newColumn == "httptunnel" | newColumn == "loadmodule" | newColumn == "perl"
           | newColumn == "ps" | newColumn == "rootkit" | newColumn == "sqlattack" | newColumn == "xterm"] = "U2R"
  
  newColumn[newColumn == "ftp_write" | newColumn == "guess_passwd" | newColumn == "imap" | newColumn == "multihop"
            | newColumn == "named" | newColumn == "phf" | newColumn == "sendmail" | newColumn == "snmpgetattack"
            | newColumn == "snmpguess" | newColumn == "spy" | newColumn == "warezclient" | newColumn == "warezmaster"
            | newColumn == "worm" | newColumn == "xlock" | newColumn == "xsnoop"] = "R2L"
  
  newColumn[newColumn == "ipsweep" | newColumn == "mscan" | newColumn == "nmap" | newColumn == "portsweep"
            | newColumn == "saint" | newColumn == "satan"] = "Probing"
  
  return(newColumn)
}

#Function that assign a label as attack or normal
NormalAttackLabel = function(dataframe)
{
  temporal = as.character(dataframe$V44)
  temporal[temporal != "normal"] = "attack"
  return(temporal)
}

#Function that give names to columns
ColumnNames = function(dataframe)
{
  colnames(dataframe) = c("Duration", "Protocol_type", "Service", "Flag", "Src_bytes",
                                 "Dst_bytes", "Land", "Wrong_fragment", "Urgent", "Hot",
                                 "Num_failed_logins", "Logged_in", "Num_compromised", "Root_shell",
                                 "Su_attempted", "Num_root", "Num_file_creations", "Num_shells",
                                 "Num_ccess_files", "Num_outbound_cmds", "Is_host_login", "Is_guest_login",
                                 "Count", "Srv_count", "Serror_rate", "Srv_error_rate", "Rerror_rate",
                                 "Srv_rerror_rate", "Same_srv_rate", "Diff_srv_rate", "Srv_diff_host_rate",
                                 "Dst_host_count", "Dst_host_srv_count", "Dst_host_same_srv_rate",
                                 "Dst_host_diff_srv_rate", "Dst_host_same_src_port_rate", "Dst_host_srv_diff_host_rate",
                                 "Dst_host_serror_rate", "Dst_host_srv_serror_rate", "Dst_host_rerror_rate",
                                 "Dst_host_srv_rerror_rate", "Label_Normal_TypeAttack", "Label_Num_Classifiers","Label_Normal_ClassAttack",
                                 "Label_Normal_or_Attack")
  
  return(dataframe)
}

#Transformation of Protocol_type feature into numeric type
ProtocolTransformation = function(dataframe)
{
  dataframe$Protocol_type = as.character(dataframe$Protocol_type)
  dataframe$Protocol_type[dataframe$Protocol_type == "icmp"] = 1
  dataframe$Protocol_type[dataframe$Protocol_type == "tcp"] = 2
  dataframe$Protocol_type[dataframe$Protocol_type == "udp"] = 3

  return(dataframe)
}

#Transformation of Service feature into numeric type
ServiceTransformation = function(dataframe)
{
  dataframe$Service = as.character(dataframe$Service)
  dataframe$Service[dataframe$Service == "aol"] = 1
  dataframe$Service[dataframe$Service == "auth"] = 2
  dataframe$Service[dataframe$Service == "bgp"] = 3
  dataframe$Service[dataframe$Service == "courier"] = 4
  dataframe$Service[dataframe$Service == "csnet_ns"] = 5
  dataframe$Service[dataframe$Service == "ctf"] = 6
  dataframe$Service[dataframe$Service == "daytime"] = 7
  dataframe$Service[dataframe$Service == "discard"] = 8
  dataframe$Service[dataframe$Service == "domain"] = 9
  dataframe$Service[dataframe$Service == "domain_u"] = 10
  dataframe$Service[dataframe$Service == "echo"] = 11
  dataframe$Service[dataframe$Service == "eco_i"] = 12
  dataframe$Service[dataframe$Service == "ecr_i"] = 13
  dataframe$Service[dataframe$Service == "efs"] = 14
  dataframe$Service[dataframe$Service == "exec"] = 15
  dataframe$Service[dataframe$Service == "finger"] = 16
  dataframe$Service[dataframe$Service == "ftp"] = 17
  dataframe$Service[dataframe$Service == "ftp_data"] = 18
  dataframe$Service[dataframe$Service == "gopher"] = 19
  dataframe$Service[dataframe$Service == "harvest"] = 20
  dataframe$Service[dataframe$Service == "hostnames"] = 21
  dataframe$Service[dataframe$Service == "http"] = 22
  dataframe$Service[dataframe$Service == "http_2784"] = 23
  dataframe$Service[dataframe$Service == "http_443"] = 24
  dataframe$Service[dataframe$Service == "http_8001"] = 25
  dataframe$Service[dataframe$Service == "imap4"] = 26
  dataframe$Service[dataframe$Service == "IRC"] = 27
  dataframe$Service[dataframe$Service == "iso_tsap"] = 28
  dataframe$Service[dataframe$Service == "klogin"] = 29
  dataframe$Service[dataframe$Service == "kshell"] = 30
  dataframe$Service[dataframe$Service == "ldap"] = 31
  dataframe$Service[dataframe$Service == "link"] = 32
  dataframe$Service[dataframe$Service == "login"] = 33
  dataframe$Service[dataframe$Service == "mtp"] = 34
  dataframe$Service[dataframe$Service == "name"] = 35
  dataframe$Service[dataframe$Service == "netbios_dgm"] = 36
  dataframe$Service[dataframe$Service == "netbios_ns"] = 37
  dataframe$Service[dataframe$Service == "netbios_ssn"] = 38
  dataframe$Service[dataframe$Service == "netstat"] = 39
  dataframe$Service[dataframe$Service == "nnsp"] = 40
  dataframe$Service[dataframe$Service == "nntp"] = 41
  dataframe$Service[dataframe$Service == "ntp_u"] = 42
  dataframe$Service[dataframe$Service == "other"] = 43
  dataframe$Service[dataframe$Service == "pm_dump"] = 44
  dataframe$Service[dataframe$Service == "pop_2"] = 45
  dataframe$Service[dataframe$Service == "pop_3"] = 46
  dataframe$Service[dataframe$Service == "printer"] = 47
  dataframe$Service[dataframe$Service == "private"] = 48
  dataframe$Service[dataframe$Service == "red_i"] = 49
  dataframe$Service[dataframe$Service == "remote_job"] = 50
  dataframe$Service[dataframe$Service == "rje"] = 51
  dataframe$Service[dataframe$Service == "shell"] = 52
  dataframe$Service[dataframe$Service == "smtp"] = 53
  dataframe$Service[dataframe$Service == "sql_net"] = 54
  dataframe$Service[dataframe$Service == "ssh"] = 55
  dataframe$Service[dataframe$Service == "sunrpc"] = 56
  dataframe$Service[dataframe$Service == "supdup"] = 57
  dataframe$Service[dataframe$Service == "systat"] = 58
  dataframe$Service[dataframe$Service == "telnet"] = 59
  dataframe$Service[dataframe$Service == "tftp_u"] = 60
  dataframe$Service[dataframe$Service == "time"] = 61
  dataframe$Service[dataframe$Service == "tim_i"] = 62
  dataframe$Service[dataframe$Service == "urh_i"] = 63
  dataframe$Service[dataframe$Service == "urp_i"] = 64
  dataframe$Service[dataframe$Service == "uucp"] = 65
  dataframe$Service[dataframe$Service == "uucp_path"] = 66
  dataframe$Service[dataframe$Service == "vmnet"] = 67
  dataframe$Service[dataframe$Service == "whois"] = 68
  dataframe$Service[dataframe$Service == "X11"] = 69
  dataframe$Service[dataframe$Service == "Z39_50"] = 70
  
  return(dataframe)
}

#Transformation of Flag feature into numeric type
FlagTransformation = function(dataframe)
{
  dataframe$Flag = as.character(dataframe$Flag)
  dataframe$Flag[dataframe$Flag == "OTH"] = 1
  dataframe$Flag[dataframe$Flag == "REJ"] = 2
  dataframe$Flag[dataframe$Flag == "RSTO"] = 3
  dataframe$Flag[dataframe$Flag == "RSTOS0"] = 4
  dataframe$Flag[dataframe$Flag == "RSTR"] = 5
  dataframe$Flag[dataframe$Flag == "S0"] = 6
  dataframe$Flag[dataframe$Flag == "S1"] = 7
  dataframe$Flag[dataframe$Flag == "S2"] = 8
  dataframe$Flag[dataframe$Flag == "S3"] = 9
  dataframe$Flag[dataframe$Flag == "SF"] = 10
  dataframe$Flag[dataframe$Flag == "SH"] = 11
  
  return(dataframe)
}

#Check if levels of features are greather than 1
CheckFeaturesLevels = function(dataframe)
{
  returnValue = vector(mode = "numeric")
  
  for (i in 1:ncol(dataframe))
  {
    if(length(unique(dataframe[,i])) == 1)
      returnValue[length(returnValue)+1] = i
  }
  
  return(returnValue)
}