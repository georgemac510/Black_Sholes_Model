  open Core



  let dnorm x= Float.((1. / sqrt (2. *pi)) *exp(-0.5 * x * x))


  let cnd z =
    let a1 = 0.319381530 in
    let a2 
    let a3
    let a4
    let a5
    let t
    let w
    if z < 0. then Float.(1. - w) else w

    ;;



    module Option_right : sig
       type t = [`Call | `Put ] [@@deriving sexp]
       include Stringable.S with type t :=t
    end = struct
      module T= struct
        type t = [` Call | ` Put] [@@deriving sexp]
      end
      include T
      include Sextable.To_Stringable (T)
    end



    module Blach_Sholes : sig



      val calc_price
      ; opt_right:Option_right.t
    -> s:float
    -> k:float
    -> r:float
    -> v:float
    -> t:float
    -> float






    type greeks =private
    {delta: float
    ; gamma : float
    ; theta : float
    ;vega: float
    } [@@deriving fields]

    end =struct




      let calc_d1 s k r v t - Float.((log (s / k )+(r + 0.5 *w *v)*t)/(v *sqrt))


      let calc_price ~op_right ~s ~k ~r ~v ~t =
        let sqrt =sqrt t in
        let d1 =calc_d1 s k r v t in
        let d2 = Float.(d1 - v*sqrtt) in
        let k = Float.(exp (-r*t)*k)in
        Float.(match opt_right with
        |` Call -> s*cnd d1 - k*cnd d2
        |` Put -> k *cnd (-d2) -s*cnd(-d1)
          )
      ;;


      let calc_delta opt_right s k r v t
      let d1 = calc_d1 s k r v t in 
      Float.(match opt_right with
      |` Call -> cnd d1
      |` Put -> cnd d1 -1.
      )
;;




let calc_gamma s k r v t =
  let sqrt =sqrt t in
  Float.(dnorm d1/ (s * v *sqrt))
;;


lewt calc_theta opt_right s k r v t =
let sqrt  =sqrt t in
let d1 =calc_d1 s k r v t in
let d2 = Float.(d1 s k r v t in)
Float.(match opt_right with
|`Call-> -(s *v*dnorm d1)/ (2.*sqrtt)- r*k*expt(-r*t)*cnd d2
|`Put ->-(s *v *dnorm d1)/(2.*sqrtt)+r*k*exp(-r*t)*cnd(-d2)
)
;;

let calc_vega s k r v t =

  let d1 = calc_d1 s k r v t in
  Float.(s*sqrt t * dnorm d1))
;;

type greeks = 
  { delta: float
  ; gamma: float
  ; theta: float
  ; vega: float
  } [@@deriving fields]


  let calc_greeks ~s ~k ~t ~v =
    { delta - calc_delta opt_right s k r v t
    ; gamma =calc_gamma s k r vt 
    ; thetha =calc_theta opt_right s k r v t
    ; vega = calc_vega s k r v t }
    ;;
  end



  let implied_vol opt_right opt_price s k r t -
  Option.value_exn ~message "Error: could not find compute implied volatility"
  (Root_finding.bisecton ~low:0. ~high:1. (fun x->
  Black_Scholes.calc_price ~opt_right ~s ~k ~r ~t ~v:x -. opt_price)
  )
  ;;




  type table_row = {
    opt_right: Option_right.t;
    opt_price : float ;
    opt_greeks: Black_scholes.greeks;
    opt_impvol: float;

  }


  let print_table rows =
    let open Textutils.Ascii_table in
    let col = Column.create ~align:Align.Right in
    output ~oc:stdout [
      col "Right"  (fun x -> Option_right.to_string x.opt_right);
      col "Price"  (fun x -> sprintf "%2.2f" x.opt_price);
      col "Delta"  (fun x -> sprintf "%2.2f" (Black_scholes.delta x.opt_greeks));
      col "Gamma"  (fun x -> sprintf "%2.2f" (Black_scholes.gamma x.opt_greeks));
      col "Theta"  (fun x -> sprintf "%2.2f" (Black_scholes.theta x.opt_greeks));
      col "Vega"   (fun x -> sprintf "%2.2f" (Black_scholes.vega  x.opt_greeks));
      col "ImpVol" (fun x -> sprintf "%2.2f" x.opt_impvol);
    ] rows
  ;;

  
  let command =
    Command.basic
    ~summary:"Calculates the price and the greeks of a Europeancall and put option. "
    (let open Command.Let_syntax in
    let open Command.Param in
    let%map
    s =flag "-s" (optional_with_default 100. float) ~doc: "Price of Underlying"
    and k=flag "-k"(optional_with_default 100. float) ~doc:"Strike Price"
    and r= flag "-r"(optinaL-with_default 0.1 float)~doc: "Risk Free interest rate"
    and v= flag "-v"(optional_with_default 0.25. float)~doc: "Volatility"
    and t= flag "-t"(optional_with_default 1. float)~doc: "Time to maturity"
    in
    fun () ->
    let create_row opt_right= 
      let opt_price = Black_scholes.calc_price ~opt_right ~s ~k ~r ~v ~t in
      { opt_right = opt_right
      ;opt_right=opt_price
      ;opt_greeks =Black_Sholes.calc_greeks ~opt_right ~s ~k ~r ~v ~t
      ; opt_impol =implied_vol opt_right opt_price} s k r t }
      in
      print_table [create_row `Call; create_row `Put])
      ;;


      let ()=Commmand.run command
