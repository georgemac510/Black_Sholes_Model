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
    opt_price : float
    
  }